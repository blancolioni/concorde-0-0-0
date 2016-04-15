with Ada.Text_IO;

with Concorde.Commodities.Db;

package body Concorde.Markets is

   ------------------------
   -- Calculate_Quantity --
   ------------------------

   function Calculate_Quantity
     (Agreed_Price : Concorde.Money.Price_Type;
      Buy_Or_Sell  : Concorde.Trades.Offer_Type;
      Offer        : Offer_Info)
      return Concorde.Quantities.Quantity
   is
      use Concorde.Money;
      use Concorde.Quantities;
      Factor : Unit_Real;
   begin
      case Buy_Or_Sell is
         when Concorde.Trades.Buy =>
            if Agreed_Price > Offer.Limit_Price then
               Factor := 0.0;
            elsif Agreed_Price > Offer.Offer_Price then
               Factor := (To_Real (Offer.Limit_Price) - To_Real (Agreed_Price))
                 / (To_Real (Offer.Limit_Price) - To_Real (Offer.Offer_Price));
            else
               Factor := 1.0;
            end if;
         when Concorde.Trades.Sell =>
            if Agreed_Price < Offer.Limit_Price then
               Factor := 0.0;
            elsif Agreed_Price < Offer.Offer_Price then
               Factor := (To_Real (Agreed_Price) - To_Real (Offer.Limit_Price))
                 / (To_Real (Offer.Offer_Price) - To_Real (Offer.Limit_Price));
            else
               Factor := 1.0;
            end if;
      end case;

      case Offer.Limit_Quantity is
         when No_Change =>
            Factor := 1.0;
         when Proportional =>
            Factor := 0.5 + Factor / 2.0;
         when Quadratic =>
            Factor := 0.5 + Factor ** 2 / 2.0;
      end case;

      declare
         Result : constant Quantity :=
                    Offer.Remaining_Quantity * To_Quantity (Factor);
      begin
         return Max (Result, Unit);
      end;

   end Calculate_Quantity;

   ---------------------
   -- Check_Commodity --
   ---------------------

   procedure Check_Commodity
     (Market    : Root_Market_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
   is
   begin
      if Market.Commodities.Element (Commodity.Reference) = null then
         declare
            New_Cached : constant Cached_Commodity :=
                           new Cached_Commodity_Record'
                             (Current_Price         => Commodity.Base_Price,
                              Historical_Mean_Price => Commodity.Base_Price,
                              Supply                => Quantities.Zero,
                              Demand                => Quantities.Zero,
                              Traded_Quantity       => Quantities.Zero,
                              Offers                => new Commodity_Offers);
         begin
            Market.Commodities.Replace_Element
              (Commodity.Reference, New_Cached);
         end;
      end if;
   end Check_Commodity;

   -------------------
   -- Create_Market --
   -------------------

   function Create_Market
     (Name           : String;
      Enable_Logging : Boolean)
      return Market_Type
   is
   begin
      return new Root_Market_Type'
        (Name        => new String'(Name),
         Commodities => new Cached_Commodity_Vectors.Vector,
         Enable_Logging => Enable_Logging);
   end Create_Market;

   ------------------
   -- Create_Offer --
   ------------------

   overriding procedure Create_Offer
     (Market    : in out Root_Market_Type;
      Offer     : Concorde.Trades.Offer_Type;
      Agent     : not null access constant
        Concorde.Trades.Trader_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity;
      Price     : Concorde.Money.Price_Type;
      Limit     : Concorde.Money.Price_Type)
   is
      Info : constant Cached_Commodity :=
               Market.Get_Commodity (Commodity);
   begin
      case Offer is
         when Concorde.Trades.Buy =>
            Info.Offers.Add_Buy_Offer (Agent, Quantity, Price, Limit);
         when Concorde.Trades.Sell =>
            Info.Offers.Add_Sell_Offer (Agent, Quantity, Price, Limit);
      end case;
   end Create_Offer;

   --------------------
   -- Current_Demand --
   --------------------

   overriding function Current_Demand
     (Market   : Root_Market_Type;
      Item     : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity
   is
   begin
      return Market.Get_Commodity (Item).Offers.Total_Demand;
   end Current_Demand;

   -------------------
   -- Current_Price --
   -------------------

   overriding function Current_Price
     (Market    : Root_Market_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type
   is
   begin
      return Market.Get_Commodity (Commodity).Current_Price;
   end Current_Price;

   --------------------
   -- Current_Supply --
   --------------------

   overriding function Current_Supply
     (Market   : Root_Market_Type;
      Item     : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity
   is
   begin
      return Market.Get_Commodity (Item).Offers.Total_Supply;
   end Current_Supply;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Market : in out Root_Market_Type'Class)
   is

      procedure Update_Commodity
        (Commodity : Concorde.Commodities.Commodity_Type);

      ----------------------
      -- Update_Commodity --
      ----------------------

      procedure Update_Commodity
        (Commodity : Concorde.Commodities.Commodity_Type)
      is
      begin
         Market.Execute_Commodity_Trades (Commodity);
      end Update_Commodity;

   begin
      Concorde.Commodities.Db.Scan (Update_Commodity'Access);
   end Execute;

   ------------------------------
   -- Execute_Commodity_Trades --
   ------------------------------

   procedure Execute_Commodity_Trades
     (Market    : in out Root_Market_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
   is
      use Concorde.Money, Concorde.Quantities;
      Info           : constant Cached_Commodity :=
                         Market.Get_Commodity (Commodity);
      Bids           : Offer_Vectors.Vector := Info.Offers.Buy_Offers;
      Asks           : Offer_Vectors.Vector := Info.Offers.Sell_Offers;
      Total_Quantity : Quantity := Zero;
      Mean_Price     : Price_Type;
      Supply         : constant Quantity := Info.Offers.Total_Supply;
      Demand         : constant Quantity := Info.Offers.Total_Demand;
      Total_Money    : Money_Type := Zero;
   begin

      if Bids.Is_Empty or else Asks.Is_Empty then
         return;
      end if;

      Offer_Sorting.Sort (Bids);
      Offer_Sorting.Sort (Asks);

      declare
         use Offer_Vectors;
         Next_Bid : Cursor := Bids.Last;
         Next_Ask : Cursor := Asks.First;
         Ask      : Offer_Info := Element (Next_Ask);
         Bid      : Offer_Info := Element (Next_Bid);
      begin
         while Has_Element (Next_Bid)
           and then Has_Element (Next_Ask)
         loop

            declare
               Ask_Price    : constant Price_Type := Ask.Current_Price;
               Bid_Price    : constant Price_Type := Bid.Current_Price;
               Price        : constant Price_Type :=
                                Adjust_Price (Ask_Price + Bid_Price, 0.5);
               Ask_Quantity : constant Quantity :=
                                Calculate_Quantity
                                  (Price, Concorde.Trades.Sell, Ask);
               Bid_Quantity : constant Quantity :=
                                Calculate_Quantity
                                  (Price, Concorde.Trades.Buy, Bid);
               Traded_Quantity : constant Quantity :=
                               Min (Ask_Quantity, Bid_Quantity);
               Money           : constant Money_Type :=
                                   Total (Price, Traded_Quantity);
            begin

               Ask.Remaining_Quantity := Ask_Quantity;
               Bid.Remaining_Quantity := Bid_Quantity;

               Market.Log
                 (Commodity.Name
                  & ": "
                  & " Ask "
                  & Image (Ask.Current_Price)
                  & "; Bid "
                  & Image (Bid.Current_Price)
                  & (if Traded_Quantity > Zero
                    then "; final contract "
                    & Image (Traded_Quantity)
                    & " @ "
                    & Image (Price)
                    else "; no sale"));

               if Traded_Quantity > Zero then
                  Ask.Remaining_Quantity :=
                    Ask.Remaining_Quantity - Traded_Quantity;
                  Bid.Remaining_Quantity :=
                    Bid.Remaining_Quantity - Traded_Quantity;

                  if Ask.Current_Price = Ask.Offer_Price then
                     Ask.Closed_At_Price :=
                       Ask.Closed_At_Price + Traded_Quantity;
                  else
                     Ask.Closed_At_Limit :=
                       Ask.Closed_At_Limit + Traded_Quantity;
                  end if;

                  if Bid.Current_Price = Bid.Offer_Price then
                     Bid.Closed_At_Price :=
                       Bid.Closed_At_Price + Traded_Quantity;
                  else
                     Bid.Closed_At_Limit :=
                       Bid.Closed_At_Limit + Traded_Quantity;
                  end if;

                  Ask.Total_Cost := Ask.Total_Cost
                    + Total (Price, Traded_Quantity);

                  Bid.Total_Cost := Bid.Total_Cost
                    + Total (Price, Traded_Quantity);

                  Bids.Replace_Element (Next_Bid, Bid);
                  Asks.Replace_Element (Next_Ask, Ask);

                  Total_Quantity := Total_Quantity + Traded_Quantity;
                  Total_Money    := Total_Money + Money;

               end if;

               if Ask.Remaining_Quantity = Zero
                 or else Price < Ask.Limit_Price
               then
                  loop
                     Next (Next_Ask);
                     if Has_Element (Next_Ask) then
                        Ask := Element (Next_Ask);
                     end if;
                     exit when not Has_Element (Next_Ask)
                       or else Ask.Agent /= Bid.Agent;
                  end loop;
               end if;
               if Bid.Remaining_Quantity = Zero
                 or else Price > Bid.Limit_Price
               then
                  loop
                     Previous (Next_Bid);
                     if Has_Element (Next_Bid) then
                        Bid := Element (Next_Bid);
                     end if;
                     exit when not Has_Element (Next_Bid)
                       or else Bid.Agent /= Ask.Agent;
                  end loop;
               end if;
            end;
         end loop;
      end;

      if Total_Quantity > Zero then
         Mean_Price := Price (Total_Money, Total_Quantity);
      else
         Mean_Price := Zero;
      end if;

      Market.Log
        (Commodity.Name
         & ": supply " & Image (Supply)
         & "; demand " & Image (Demand)
         & "; total sold " & Image (Total_Quantity)
         & "; total value " & Image (Total_Money)
         & "; average price " & Image (Mean_Price));

      if Mean_Price > Zero then
         Info.Current_Price := Mean_Price;
         Info.Historical_Mean_Price :=
           Adjust_Price
             (Info.Historical_Mean_Price + Info.Current_Price, 0.5);
      end if;

      Info.Supply := Supply;
      Info.Demand := Demand;
      Info.Traded_Quantity := Total_Quantity;

--        Conflict.Db.Market_Commodity.Create
--          (Market           => Market,
--           Date             => Conflict.Dates.Today,
--           Commodity        => Commodity,
--           Historical_Price => Info.Historical_Mean_Price,
--           Average_Price    => Info.Current_Price,
--           Traded_Quantity  => Info.Traded_Quantity,
--           Supply           => Info.Supply,
--           Demand           => Info.Demand);
--
--        for Bid of Bids loop
--           Conflict.Agents.Update_Buyer_Price_Belief
--             (Agent            => Bid.Agent,
--              Commodity        => Commodity,
--              Total_Bought     => Info.Traded_Quantity,
--              Total_Supply     => Info.Supply,
--              Total_Demand     => Info.Demand,
--              Average_Price    => Mean_Price,
--              Historical_Price => Info.Historical_Mean_Price,
--              Agent_Price      => Bid.Offer_Price,
--              Agent_Offered    => Bid.Offered_Quantity,
--              Agent_Bought     => Bid.Closed_At_Price + Bid.Closed_At_Limit,
--              Total_Expense    => Bid.Total_Cost);
--        end loop;
--
--        for Ask of Asks loop
--           Conflict.Agents.Update_Seller_Price_Belief
--             (Agent            => Ask.Agent,
--              Commodity        => Commodity,
--              Total_Sold       => Info.Traded_Quantity,
--              Total_Supply     => Info.Supply,
--              Total_Demand     => Info.Demand,
--              Average_Price    => Mean_Price,
--              Historical_Price => Info.Historical_Mean_Price,
--              Agent_Price      => Ask.Offer_Price,
--              Agent_Offered    => Ask.Offered_Quantity,
--              Agent_Sold       => Ask.Closed_At_Price + Ask.Closed_At_Limit,
--              Total_Income     => Ask.Total_Cost);
--        end loop;

      Info.Offers.Clear;

      --        for Agent of
      --          Conflict.Db.Agent.Select_By_Market (Market)
      --        loop
      --           declare
      --              Production : Db.Agent_Production.Agent_Production_Type :=
      --                             Conflict.Db.Agent_Production.Get_By_Agent
      --                               (Agent.Reference);
      --           begin
      --              if Production.Has_Element then
      --                 declare
      --                    use Conflict.Db.Market_Production;
      --                    Market_Prod : Market_Production_Type :=
      --                                    Get_By_Market_Production_Date
      --                                      (Market,
      --                                       Production.Production,
      --                                       Today);
      --                 begin
      --                    if Market_Prod.Has_Element then
      --                       Market_Prod.Set_Producer_Count
      --                         (Market_Prod.Producer_Count + 1);
      --                       Market_Prod.Set_Total_Cash
      --                         (Market_Prod.Total_Cash + Agent.Cash);
      --                    else
      --                       Conflict.Db.Market_Production.Create
      --                         (Market         => Market,
      --                          Date           => Today,
      --                          Production     => Production.Production,
      --                          Producer_Count => 1,
      --                          Total_Spent    => 0.0,
      --                          Total_Earned   => 0.0,
      --                          Total_Cash     => Agent.Cash);
      --                    end if;
      --                 end;
      --              end if;
      --           end;
      --        end loop;

   end Execute_Commodity_Trades;

   -------------------
   -- Get_Commodity --
   -------------------

   function Get_Commodity
     (Market    : Root_Market_Type'Class;
      Commodity : Commodities.Commodity_Type)
      return Cached_Commodity
   is
   begin
      Market.Check_Commodity (Commodity);
      return Market.Commodities.Element (Commodity.Reference);
   end Get_Commodity;

   ---------------------------
   -- Historical_Mean_Price --
   ---------------------------

   overriding function Historical_Mean_Price
     (Market    : Root_Market_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type
   is
   begin
      return Market.Get_Commodity (Commodity).Historical_Mean_Price;
   end Historical_Mean_Price;

   -----------------
   -- Last_Demand --
   -----------------

   overriding function Last_Demand
     (Market   : Root_Market_Type;
      Item     : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity
   is
   begin
      return Market.Get_Commodity (Item).Demand;
   end Last_Demand;

   -----------------
   -- Last_Supply --
   -----------------

   overriding function Last_Supply
     (Market   : Root_Market_Type;
      Item     : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity
   is
   begin
      return Market.Get_Commodity (Item).Supply;
   end Last_Supply;

   ---------
   -- Log --
   ---------

   overriding procedure Log
     (Market  : Root_Market_Type;
      Message : String)
   is
   begin
      if Market.Enable_Logging then
         Ada.Text_IO.Put_Line (Message);
      end if;
   end Log;

   ----------------------
   -- Commodity_Offers --
   ----------------------

   protected body Commodity_Offers is

      -------------------
      -- Add_Buy_Offer --
      -------------------

      procedure Add_Buy_Offer
        (Agent     : not null access constant
           Concorde.Trades.Trader_Interface'Class;
         Quantity  : Concorde.Quantities.Quantity;
         Price     : Concorde.Money.Price_Type;
         Limit     : Concorde.Money.Price_Type)
      is
      begin
         Buys.Append (Make_Offer (Agent, Quantity, Price, Limit));
      end Add_Buy_Offer;

      --------------------
      -- Add_Sell_Offer --
      --------------------

      procedure Add_Sell_Offer
        (Agent     : not null access constant
           Concorde.Trades.Trader_Interface'Class;
         Quantity  : Concorde.Quantities.Quantity;
         Price     : Concorde.Money.Price_Type;
         Limit     : Concorde.Money.Price_Type)
      is
      begin
         Sells.Append (Make_Offer (Agent, Quantity, Price, Limit));
      end Add_Sell_Offer;

      ----------------
      -- Buy_Offers --
      ----------------

      function Buy_Offers return Offer_Vectors.Vector is
      begin
         return Buys;
      end Buy_Offers;

      -----------
      -- Clear --
      -----------

      procedure Clear is
      begin
         Buys.Clear;
         Sells.Clear;
      end Clear;

      -----------------
      -- Sell_Offers --
      -----------------

      function Sell_Offers return Offer_Vectors.Vector is
      begin
         return Sells;
      end Sell_Offers;

      ------------------
      -- Total_Demand --
      ------------------

      function Total_Demand return Concorde.Quantities.Quantity is
         use Concorde.Quantities;
         Result : Quantity := Zero;
      begin
         for Offer of Buys loop
            Result := Result + Offer.Offered_Quantity;
         end loop;
         return Result;
      end Total_Demand;

      ------------------
      -- Total_Supply --
      ------------------

      function Total_Supply return Concorde.Quantities.Quantity is
         use Concorde.Quantities;
         Result : Quantity := Zero;
      begin
         for Offer of Sells loop
            Result := Result + Offer.Offered_Quantity;
         end loop;
         return Result;
      end Total_Supply;

   end Commodity_Offers;

end Concorde.Markets;
