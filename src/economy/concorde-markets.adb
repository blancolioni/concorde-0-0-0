with Ada.Text_IO;
with Ada.Exceptions;

with Concorde.Paths;

with Concorde.Dates;

with Concorde.Commodities.Db;
with Concorde.Markets.Db;

package body Concorde.Markets is

   function Taxable_Image
     (Price_With_Tax : Concorde.Money.Price_Type;
      Tax_Rate       : Non_Negative_Real)
      return String;

   -------------------
   -- After_Trading --
   -------------------

   procedure After_Trading
     (Market : in out Root_Market_Type'Class)
   is
      procedure Clear_Offers
        (Commodity : Concorde.Commodities.Commodity_Type);

      ------------------
      -- Clear_Offers --
      ------------------

      procedure Clear_Offers
        (Commodity : Concorde.Commodities.Commodity_Type)
      is
         Item : constant Cached_Commodity :=
                  Market.Commodities.Element (Commodity.Reference);
      begin
         if Item /= null then
            Item.Offers.Clear;
            Item.Supply := Quantities.Zero;
            Item.Demand := Quantities.Zero;
            Item.Local_Supply := Quantities.Zero;
            Item.Local_Demand := Quantities.Zero;
            Item.Traded_Quantity := Quantities.Zero;
         end if;
      end Clear_Offers;

   begin
      Concorde.Commodities.Db.Scan (Clear_Offers'Access);
   end After_Trading;

   --------------------
   -- Before_Trading --
   --------------------

   procedure Before_Trading
     (Market : in out Root_Market_Type'Class)
   is null;

   ------------------------
   -- Calculate_Quantity --
   ------------------------

   function Calculate_Quantity
     (Agreed_Price : Concorde.Money.Price_Type;
      Buy_Or_Sell  : Concorde.Trades.Offer_Type;
      Commodity    : Concorde.Commodities.Commodity_Type;
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
            elsif Agreed_Price > Offer.Offer_Price  then
               Factor := (To_Real (Offer.Limit_Price - Agreed_Price))
                 / (To_Real (Offer.Limit_Price - Offer.Offer_Price));
            else
               Factor := 1.0;
            end if;
         when Concorde.Trades.Sell =>
            if Agreed_Price < Offer.Limit_Price then
               Factor := 0.0;
            elsif Agreed_Price < Offer.Offer_Price then
               Factor := (To_Real (Agreed_Price - Offer.Limit_Price))
                 / (To_Real (Offer.Offer_Price - Offer.Limit_Price));
            else
               Factor := 1.0;
            end if;
      end case;

      if Factor > 0.0 then
         case Offer.Limit_Quantity is
         when No_Change =>
            Factor := 1.0;
         when Proportional =>
            Factor := 0.5 + Factor / 2.0;
         when Quadratic =>
            Factor := 0.5 + Factor ** 2 / 2.0;
         end case;
      end if;

      declare
         Adjusted_Result : constant Quantity :=
                             Scale (Offer.Remaining_Quantity, Factor);
         Maximum_Result  : constant Quantity :=
                             Offer.Agent.Maximum_Offer_Quantity
                               (Buy_Or_Sell, Commodity);
      begin
         return Min (Max (Adjusted_Result, Unit), Maximum_Result);
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
                              Last_Average_Ask      => Commodity.Base_Price,
                              Last_Average_Bid      => Commodity.Base_Price,
                              Supply                => Quantities.Zero,
                              Demand                => Quantities.Zero,
                              Local_Supply          => Quantities.Zero,
                              Local_Demand          => Quantities.Zero,
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
     (Owner  : not null access constant
        Concorde.Objects.Named_Object_Interface'Class;
      Manager : not null access constant
        Concorde.Trades.Trade_Manager_Interface'Class;
      Enable_Logging : Boolean)
      return Market_Type
   is

      procedure Create
        (Market : in out Root_Market_Type'Class);

      procedure Create
        (Market : in out Root_Market_Type'Class)
      is
      begin
         Market.Owner := Owner;
         Market.Manager := Manager;
         Market.Commodities := new Cached_Commodity_Vectors.Vector;
         Market.Enable_Logging := Enable_Logging;
      end Create;

   begin
      return Concorde.Markets.Db.Create (Create'Access);
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

   --------------------------
   -- Current_Local_Demand --
   --------------------------

   overriding function Current_Local_Demand
     (Market   : Root_Market_Type;
      Item     : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity
   is
   begin
      return Market.Get_Commodity (Item).Offers.Local_Demand;
   end Current_Local_Demand;

   --------------------------
   -- Current_Local_Supply --
   --------------------------

   overriding function Current_Local_Supply
     (Market   : Root_Market_Type;
      Item     : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity
   is
   begin
      return Market.Get_Commodity (Item).Offers.Local_Supply;
   end Current_Local_Supply;

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

   --------------------
   -- Enable_Logging --
   --------------------

   procedure Enable_Logging
     (Market  : in out Root_Market_Type'Class;
      Enabled : Boolean := True)
   is
   begin
      Market.Enable_Logging := Enabled;
   end Enable_Logging;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Market  : in out Root_Market_Type'Class;
      Manager : in out Concorde.Trades.Trade_Manager_Interface'Class)
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
         Market.Execute_Commodity_Trades (Manager, Commodity);
      end Update_Commodity;

      File : Ada.Text_IO.File_Type;

   begin
      if Market.Enable_Logging then
         Ada.Text_IO.Create
           (File, Ada.Text_IO.Out_File,
            Concorde.Paths.Config_Path
            & "/../log/markets/"
            & Market.Name
            & "-"
            & Concorde.Dates.Current_Date_To_String
            & ".txt");
         Ada.Text_IO.Set_Output (File);
      end if;

      Concorde.Commodities.Db.Scan (Update_Commodity'Access);

      if Market.Enable_Logging then
         Ada.Text_IO.Set_Output
           (Ada.Text_IO.Standard_Output);
         Ada.Text_IO.Close (File);
      end if;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           ("exception: " & Ada.Exceptions.Exception_Message (E));
         if Market.Enable_Logging then
            Ada.Text_IO.Set_Output
              (Ada.Text_IO.Standard_Output);
            Ada.Text_IO.Close (File);
         end if;

         raise;
   end Execute;

   ------------------------------
   -- Execute_Commodity_Trades --
   ------------------------------

   procedure Execute_Commodity_Trades
     (Market    : in out Root_Market_Type'Class;
      Manager   : in out Concorde.Trades.Trade_Manager_Interface'Class;
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
      Total_Ask      : Money_Type := Zero;
      Total_Bid      : Money_Type := Zero;

   begin

      if Bids.Is_Empty or else Asks.Is_Empty then
         return;
      end if;

      Offer_Sorting.Sort (Bids);
      Offer_Sorting.Sort (Asks);

      for Ask of Asks loop
         Total_Ask := Total_Ask
           + Total (Ask.Current_Price, Ask.Offered_Quantity);
      end loop;

      for Bid of Bids loop
         Total_Bid := Total_Bid
           + Total (Bid.Current_Price, Bid.Offered_Quantity);
      end loop;

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
               Sales_Tax_Rate    : constant Unit_Real :=
                                     Manager.Tax_Rate
                                       (Concorde.Trades.Sales, Commodity);
               Export_Tax_Rate   : constant Unit_Real :=
                                     (if not Bid.Agent.Market_Resident
                                      then Manager.Tax_Rate
                                        (Concorde.Trades.Export, Commodity)
                                      else 0.0);
               Import_Tax_Rate   : constant Unit_Real :=
                                     (if not Ask.Agent.Market_Resident
                                      then Manager.Tax_Rate
                                        (Concorde.Trades.Import, Commodity)
                                      else 0.0);
               Seller_Tax_Rate   : constant Non_Negative_Real :=
                                     Sales_Tax_Rate + Import_Tax_Rate
                                       + Export_Tax_Rate;
               Buyer_Tax_Rate    : constant Non_Negative_Real := 0.0;
               Seller_Price      : constant Price_Type := Ask.Current_Price;
               Buyer_Price       : constant Price_Type := Bid.Current_Price;
               Ask_Price         : constant Price_Type := Seller_Price;
               Bid_Price         : constant Price_Type := Buyer_Price;
               Price_With_Tax    : constant Price_Type :=
                                     Adjust_Price (Ask_Price + Bid_Price, 0.5);
               Price_Without_Tax : constant Price_Type :=
                                     Without_Tax (Price_With_Tax,
                                                  Seller_Tax_Rate);
               pragma Unreferenced (Price_Without_Tax);
               Ask_Quantity      : constant Quantity :=
                                     Calculate_Quantity
                                       (Price_With_Tax,
                                        Concorde.Trades.Sell,
                                        Commodity,  Ask);
               Bid_Quantity      : constant Quantity :=
                                     Calculate_Quantity
                                       (Price_With_Tax, Concorde.Trades.Buy,
                                        Commodity, Bid);
               Traded_Quantity   : constant Quantity :=
                                     Min (Ask_Quantity, Bid_Quantity);
               Money_With_Tax    : constant Money_Type :=
                                     Total (Price_With_Tax, Traded_Quantity);
               Money_Without_Tax : constant Money_Type :=
                                     Without_Tax
                                       (Money_With_Tax, Seller_Tax_Rate);
            begin

               Ask.Remaining_Quantity := Ask_Quantity;
               Bid.Remaining_Quantity := Bid_Quantity;

               Market.Log
                 (Commodity.Name
                  & ": "
                  & Ask.Agent.Short_Name
                  & " asks "
                  & Taxable_Image (Ask_Price, Seller_Tax_Rate)
                  & " limit "
                  & Taxable_Image (Ask.Limit_Price, Seller_Tax_Rate)
                  & " for "
                  & Image (Ask_Quantity)
                  & " units"
                  & "; "
                  & Bid.Agent.Short_Name
                  & " bids "
                  & Taxable_Image (Buyer_Price, Buyer_Tax_Rate)
                  & " limit "
                  & Taxable_Image (Bid.Limit_Price, Buyer_Tax_Rate)
                  & " for "
                  & Image (Bid_Quantity)
                  & " units"
                  & (if Traded_Quantity > Zero
                    then "; final contract "
                    & Image (Traded_Quantity)
                    & " @ "
                    & Taxable_Image (Price_With_Tax, Seller_Tax_Rate)
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

                  Ask.Total_Cost := Ask.Total_Cost + Money_Without_Tax;
                  Bid.Total_Cost := Bid.Total_Cost + Money_With_Tax;

                  Ask.Agent.Execute_Trade
                    (Concorde.Trades.Sell, Commodity,
                     Traded_Quantity, Money_Without_Tax);
                  Bid.Agent.Execute_Trade
                    (Concorde.Trades.Buy, Commodity,
                     Traded_Quantity, Money_With_Tax);

                  Manager.Tax_Receipt
                    (Commodity => Commodity,
                     Quantity  => Traded_Quantity,
                     Price     => Price_With_Tax,
                     Category  => Concorde.Trades.Sales,
                     Receipt   => Money_With_Tax - Money_Without_Tax);

                  Market.Log (Commodity.Name
                              & ": tax is "
                              & Image (Money_With_Tax - Money_Without_Tax));

                  Bids.Replace_Element (Next_Bid, Bid);
                  Asks.Replace_Element (Next_Ask, Ask);

                  Total_Quantity := Total_Quantity + Traded_Quantity;
                  Total_Money    := Total_Money + Money_With_Tax;

               end if;

               if Ask.Remaining_Quantity = Zero
                 or else Price_With_Tax < Ask.Limit_Price
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
                 or else Price_With_Tax > Bid.Limit_Price
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

         while Has_Element (Next_Ask) loop
            if Element (Next_Ask).Remaining_Quantity > Zero then
               Market.Log_Offer
                 ("failed to sell", Commodity, Element (Next_Ask));
            end if;
            Next (Next_Ask);
         end loop;

         while Has_Element (Next_Bid) loop
            if Element (Next_Bid).Remaining_Quantity > Zero then
               Market.Log_Offer
                 ("failed to buy", Commodity, Element (Next_Bid));
            end if;
            Next (Next_Bid);
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

      if Total_Quantity > Zero then
         Info.Last_Average_Ask := Price (Total_Ask, Supply);
         Info.Last_Average_Bid := Price (Total_Bid, Demand);
      end if;

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
      for Bid of Bids loop
         if Bid.Agent.Belief_Based_Strategy (Commodity) then
            Bid.Agent.Update_Price_Belief
              (Trade            => Market,
               Offer            => Concorde.Trades.Buy,
               Commodity        => Commodity,
               Total_Traded     => Info.Traded_Quantity,
               Total_Supply     => Info.Supply,
               Total_Demand     => Info.Demand,
               Average_Price    => Mean_Price,
               Historical_Price => Info.Historical_Mean_Price,
               Trader_Price     => Bid.Offer_Price,
               Trader_Offered   => Bid.Offered_Quantity,
               Trader_Traded    => Bid.Closed_At_Price + Bid.Closed_At_Limit,
               Total_Money      => Bid.Total_Cost);
         end if;
      end loop;

      for Ask of Asks loop
         if Ask.Agent.Belief_Based_Strategy (Commodity) then
            Ask.Agent.Update_Price_Belief
              (Trade            => Market,
               Offer            => Concorde.Trades.Sell,
               Commodity        => Commodity,
               Total_Traded     => Info.Traded_Quantity,
               Total_Supply     => Info.Supply,
               Total_Demand     => Info.Demand,
               Average_Price    => Mean_Price,
               Historical_Price => Info.Historical_Mean_Price,
               Trader_Price     => Ask.Offer_Price,
               Trader_Offered   => Ask.Offered_Quantity,
               Trader_Traded    => Ask.Closed_At_Price + Ask.Closed_At_Limit,
               Total_Money      => Ask.Total_Cost);
         end if;
      end loop;

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

   -------------------
   -- Initial_Price --
   -------------------

   procedure Initial_Price
     (Market    : in out Root_Market_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Price     : Concorde.Money.Price_Type)
   is
      Info : constant Cached_Commodity := Market.Get_Commodity (Commodity);
   begin
      Info.Current_Price := Price;
      Info.Historical_Mean_Price := Price;
   end Initial_Price;

   ----------------------
   -- Last_Average_Ask --
   ----------------------

   overriding function Last_Average_Ask
     (Market    : Root_Market_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type
   is
   begin
      return Market.Get_Commodity (Commodity).Last_Average_Ask;
   end Last_Average_Ask;

   ----------------------
   -- Last_Average_Bid --
   ----------------------

   overriding function Last_Average_Bid
     (Market    : Root_Market_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type
   is
   begin
      return Market.Get_Commodity (Commodity).Last_Average_Bid;
   end Last_Average_Bid;

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

   ---------------
   -- Log_Offer --
   ---------------

   procedure Log_Offer
     (Market    : Root_Market_Type'Class;
      Message   : String;
      Commodity : Concorde.Commodities.Commodity_Type;
      Offer     : Offer_Info)
   is
      use Concorde.Money;
      use Concorde.Quantities;
   begin
      Market.Log
        (Message
         & ": "
         & Offer.Agent.Short_Name
         & ": "
         & Image (Offer.Remaining_Quantity)
         & " "
         & Commodity.Name
         & " @ "
         & Image (Offer.Current_Price)
         & " each");
   end Log_Offer;

   ----------
   -- Name --
   ----------

   function Name
     (Market : Root_Market_Type'Class)
      return String
   is
   begin
      return Market.Owner.Name;
   end Name;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Market : Root_Market_Type)
      return Memor.Root_Database_Type'Class
   is
      pragma Unreferenced (Market);
   begin
      return Db.Get_Database;
   end Object_Database;

   -------------------
   -- Taxable_Image --
   -------------------

   function Taxable_Image
     (Price_With_Tax : Concorde.Money.Price_Type;
      Tax_Rate       : Non_Negative_Real)
      return String
   is
      use Concorde.Money;
      Price_Without_Tax : constant Price_Type :=
                            Without_Tax (Price_With_Tax, Tax_Rate);
      Tax               : constant Price_Type :=
                            Price_With_Tax
                              - Price_Without_Tax;
   begin
      return Image (Price_With_Tax)
        & " (" & Image (Price_Without_Tax)
        & " + " & Image (Tax) & ")";
   end Taxable_Image;

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

      ------------------
      -- Local_Demand --
      ------------------

      function Local_Demand return Concorde.Quantities.Quantity is
         use Concorde.Quantities;
         Result : Quantity := Zero;
      begin
         for Offer of Buys loop
            if Offer.Agent.Market_Resident then
               Result := Result + Offer.Offered_Quantity;
            end if;
         end loop;
         return Result;
      end Local_Demand;

      ------------------
      -- Local_Supply --
      ------------------

      function Local_Supply return Concorde.Quantities.Quantity is
         use Concorde.Quantities;
         Result : Quantity := Zero;
      begin
         for Offer of Sells loop
            if Offer.Agent.Market_Resident then
               Result := Result + Offer.Offered_Quantity;
            end if;
         end loop;
         return Result;
      end Local_Supply;

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

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Market         : Root_Market_Type;
      Perform_Update : not null access
        procedure (M : in out Concorde.Trades.Trade_Interface'Class))
   is

      procedure Go
        (Market : in out Root_Market_Type'Class);

      --------
      -- Go --
      --------

      procedure Go
        (Market : in out Root_Market_Type'Class)
      is
      begin
         Perform_Update (Market);
      end Go;

   begin
      Concorde.Markets.Db.Update (Market.Reference, Go'Access);
   end Update;

end Concorde.Markets;
