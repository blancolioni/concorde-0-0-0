with Concorde.Logging;

package body Concorde.Transactions is

   procedure Add_Agent
     (Maps     : in out Agent_Maps.Map;
      Agent    : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Offered  : Concorde.Quantities.Quantity_Type;
      Budget   : Concorde.Money.Money_Type);

   procedure Add_Offer
     (List            : in out Offers_At_Price_Lists.List;
      Agent           : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Quantity        : Concorde.Quantities.Quantity_Type;
      Price           : Concorde.Money.Price_Type;
      Tax             : Unit_Real;
      Tax_Category    : Concorde.Trades.Market_Tax_Category;
      Cost            : Concorde.Money.Money_Type);

   ---------------
   -- Add_Agent --
   ---------------

   procedure Add_Agent
     (Maps     : in out Agent_Maps.Map;
      Agent    : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Offered  : Concorde.Quantities.Quantity_Type;
      Budget   : Concorde.Money.Money_Type)
   is
   begin
      Maps.Insert
        (Key      => Agent.Identifier,
         New_Item =>
           Agent_Record'
             (Agent   => Agent,
              Offered => Offered,
              Closed  => Concorde.Quantities.Zero,
              Budget  => Budget,
              Value   => Concorde.Money.Zero));
   end Add_Agent;

   -------------
   -- Add_Ask --
   -------------

   procedure Add_Ask
     (List            : in out Transaction_Request_List'Class;
      Agent           : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Quantity        : Concorde.Quantities.Quantity_Type;
      Price           : Concorde.Money.Price_Type;
      Tax             : Unit_Real;
      Tax_Category    : Concorde.Trades.Market_Tax_Category;
      Minimum_Revenue : Concorde.Money.Money_Type)
   is
      use type Concorde.Quantities.Quantity_Type;
   begin
      Add_Offer
        (List.Asks, Agent, Quantity, Price, Tax, Tax_Category,
         Minimum_Revenue);
      Add_Agent (List.Askers, Agent, Quantity, Minimum_Revenue);
      List.Total_Asks := List.Total_Asks + Quantity;
   end Add_Ask;

   -------------
   -- Add_Bid --
   -------------

   procedure Add_Bid
     (List            : in out Transaction_Request_List'Class;
      Agent           : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Quantity        : Concorde.Quantities.Quantity_Type;
      Price           : Concorde.Money.Price_Type;
      Tax             : Unit_Real;
      Tax_Category    : Concorde.Trades.Market_Tax_Category;
      Maximum_Cost    : Concorde.Money.Money_Type)
   is
      use type Concorde.Quantities.Quantity_Type;
   begin
      Add_Offer
        (List.Bids, Agent, Quantity, Price,
         Tax, Tax_Category, Maximum_Cost);
      Add_Agent (List.Bidders, Agent, Quantity, Maximum_Cost);
      List.Total_Bids := List.Total_Bids + Quantity;
   end Add_Bid;

   ---------------
   -- Add_Offer --
   ---------------

   procedure Add_Offer
     (List            : in out Offers_At_Price_Lists.List;
      Agent           : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Quantity        : Concorde.Quantities.Quantity_Type;
      Price           : Concorde.Money.Price_Type;
      Tax             : Unit_Real;
      Tax_Category    : Concorde.Trades.Market_Tax_Category;
      Cost            : Concorde.Money.Money_Type)
   is
      use Concorde.Money, Concorde.Quantities;

      Rec : constant Offer_Record := Offer_Record'
        (Agent        => Agent,
         Quantity     => Quantity,
         Price        => Price,
         Tax          => Tax,
         Tax_Category => Tax_Category,
         Cost         => Cost);
   begin
      for Position in List.Iterate loop
         declare
            Item_Price : constant Price_Type :=
                           List (Position).Price;
         begin
            if Price = Item_Price then
               declare
                  Offer : Offers_At_Price_Record renames
                            List (Position);
               begin
                  Offer.Offers.Append (Rec);
                  Offer.Total := Offer.Total + Rec.Quantity;
                  return;
               end;
            elsif Price < Item_Price then
               declare
                  Offer : Offers_At_Price_Record := Offers_At_Price_Record'
                    (Price  => Price,
                     Total  => Rec.Quantity,
                     Offers => <>);
               begin
                  Offer.Offers.Append (Rec);
                  List.Insert (Position, Offer);
                  return;
               end;
            end if;
         end;
      end loop;

      declare
         Offer : Offers_At_Price_Record := Offers_At_Price_Record'
           (Price  => Price,
            Total  => Rec.Quantity,
            Offers => <>);
      begin
         Offer.Offers.Append (Rec);
         List.Append (Offer);
      end;
   end Add_Offer;

   --------------------------
   -- Execute_Transactions --
   --------------------------

   procedure Execute_Transactions
     (List : in out Transaction_Request_List'Class)
   is
      use Concorde.Money;
      use Concorde.Quantities;
      use Offers_At_Price_Lists;
      Total_Traded : Quantity_Type := Zero;
      Total_Value  : Money_Type    := Zero;
      Current_Ask  : Cursor := List.Asks.First;
      Current_Bid  : Cursor := List.Bids.Last;

      procedure Create_New_Offers
        (Ask_Offers : in out Offers_At_Price_Record;
         Bid_Offers : in out Offers_At_Price_Record;
         New_Asks   : in out Offers_At_Price_Lists.List;
         New_Bids   : in out Offers_At_Price_Lists.List);

      procedure Execute_Offers_At_Price
        (Ask_Offers : in out Offers_At_Price_Record;
         Bid_Offers : in out Offers_At_Price_Record);

      -----------------------
      -- Create_New_Offers --
      -----------------------

      procedure Create_New_Offers
        (Ask_Offers : in out Offers_At_Price_Record;
         Bid_Offers : in out Offers_At_Price_Record;
         New_Asks   : in out Offers_At_Price_Lists.List;
         New_Bids   : in out Offers_At_Price_Lists.List)
      is
      begin
         for Ask of Ask_Offers.Offers loop
            if Ask.Quantity > Zero then
               declare
                  Remaining_Value : constant Money_Type := Ask.Cost;
                  Minimum_Ask     : constant Price_Type :=
                                      Add_Tax
                                        (Max
                                           (Adjust_Price (Ask.Price, 0.5),
                                            Price
                                              (Remaining_Value,
                                               Ask.Quantity)),
                                         Ask.Tax);
               begin
                  if Minimum_Ask < Ask.Price then
                     Concorde.Logging.Log
                       ("transaction", "-", List.Commodity.Name,
                        Ask.Agent.Identifier
                        & " creates new ask "
                        & Show (Ask.Quantity)
                        & " at "
                        & Show (Minimum_Ask)
                        & " ("
                        & Show (Without_Tax (Minimum_Ask, Ask.Tax))
                        & " without tax)"
                        & " ea");
                     Add_Offer
                       (List         => New_Asks,
                        Agent        => Ask.Agent,
                        Quantity     => Ask.Quantity,
                        Price        => Minimum_Ask,
                        Tax          => Ask.Tax,
                        Tax_Category => Ask.Tax_Category,
                        Cost         => Remaining_Value);
                  end if;
               end;
            end if;
         end loop;
         for Bid of Bid_Offers.Offers loop
            if Bid.Quantity > Zero then
               declare
                  Remaining_Budget : constant Money_Type := Bid.Cost;
                  Maximum_Price    : constant Price_Type :=
                                      Min
                                         (Adjust_Price (Bid.Price, 2.0),
                                          Price
                                            (Remaining_Budget,
                                             Bid.Quantity));
               begin
                  if Maximum_Price > Bid.Price then
                     Concorde.Logging.Log
                       ("transaction", "-", List.Commodity.Name,
                        Bid.Agent.Identifier
                        & " creates new bid "
                        & Show (Bid.Quantity)
                        & " at "
                        & Show (Maximum_Price)
                        & " ("
                        & Show (Without_Tax (Maximum_Price, Bid.Tax))
                        & " without tax)"
                        & " ea");
                     Add_Offer
                       (List         => New_Bids,
                        Agent        => Bid.Agent,
                        Quantity     => Bid.Quantity,
                        Price        => Maximum_Price,
                        Tax          => Bid.Tax,
                        Tax_Category => Bid.Tax_Category,
                        Cost         => Remaining_Budget);
                  end if;
               end;
            end if;
         end loop;
      end Create_New_Offers;

      -----------------------------
      -- Execute_Offers_At_Price --
      -----------------------------

      procedure Execute_Offers_At_Price
        (Ask_Offers : in out Offers_At_Price_Record;
         Bid_Offers : in out Offers_At_Price_Record)
      is
         Ask_Total : constant Quantity_Type := Ask_Offers.Total;
         Bid_Total : constant Quantity_Type := Bid_Offers.Total;
         Ask_Scale : constant Unit_Real :=
                       (if Ask_Total >= Bid_Total then 1.0
                        else To_Real (Ask_Total) / To_Real (Bid_Total));
         Bid_Scale : constant Unit_Real :=
                       (if Bid_Total >= Ask_Total then 1.0
                        else To_Real (Bid_Total) / To_Real (Ask_Total));
         Price     : constant Price_Type :=
                       Adjust_Price (Ask_Offers.Price + Bid_Offers.Price, 0.5);

      begin

         Concorde.Logging.Log
           ("transaction", "-", List.Commodity.Name,
            "executing offers at "
            & Show (Price)
            & ": asks = " & Show (Ask_Total)
            & " (" & Natural'Image (Natural (Ask_Scale * 100.0)) & "%)"
            & "; bids = " & Show (Bid_Total)
            & " (" & Natural'Image (Natural (Bid_Scale * 100.0)) & "%)");

         for Ask of Ask_Offers.Offers loop
            declare
               Traded : constant Quantity_Type :=
                          Scale (Ask.Quantity, Bid_Scale);
               Value  : constant Money_Type :=
                          Total (Price, Traded);
            begin
               Concorde.Logging.Log
                 ("transaction", "-", List.Commodity.Name,
                  Ask.Agent.Identifier
                  & " sells "
                  & Show (Traded)
                  & " of "
                  & Show (Ask.Quantity)
                  & " at "
                  & Show (Price)
                  & " (" & Show (Without_Tax (Price, Ask.Tax))
                  & " without tax)"
                  & " ea, total "
                  & Show (Value));
               Total_Traded := Total_Traded + Traded;
               Total_Value := Total_Value + Value;

               Ask.Agent.Variable_Reference.On_Commodity_Sell
                 (Commodity => List.Commodity,
                  Quantity  => Traded,
                  Price     => Without_Tax (Price, Ask.Tax));

               List.Total_Tax (Ask.Tax_Category) :=
                 List.Total_Tax (Ask.Tax_Category)
                 + Total (Price - Without_Tax (Price, Ask.Tax), Traded);

               Ask.Quantity := Ask.Quantity - Traded;
               if Value > Ask.Cost then
                  Ask.Cost := Zero;
               else
                  Ask.Cost := Ask.Cost - Value;
               end if;

               Ask_Offers.Total := Ask_Offers.Total - Traded;
            end;
         end loop;

         for Bid of Bid_Offers.Offers loop
            declare
               Traded : constant Quantity_Type :=
                          Scale (Bid.Quantity, Ask_Scale);
               Value  : constant Money_Type :=
                          Total (Bid.Price, Bid.Quantity);
            begin
               Concorde.Logging.Log
                 ("transaction", "-", List.Commodity.Name,
                  Bid.Agent.Identifier
                  & " buys "
                  & Show (Traded)
                  & " of "
                  & Show (Bid.Quantity)
                  & " at "
                  & Show (Price)
                  & " ("
                  & Show (Without_Tax (Price, Bid.Tax))
                  & " without tax)"
                  & " ea, total "
                  & Show (Value));

               Bid.Agent.Variable_Reference.On_Commodity_Buy
                 (Commodity => List.Commodity,
                  Quantity  => Traded,
                  Price     => Price);

               List.Total_Tax (Bid.Tax_Category) :=
                 List.Total_Tax (Bid.Tax_Category)
                 + Total (Price - Without_Tax (Price, Bid.Tax), Traded);

               Bid.Quantity := Bid.Quantity - Traded;
               Bid_Offers.Total := Bid_Offers.Total - Traded;
            end;
         end loop;
      end Execute_Offers_At_Price;

   begin
      while Has_Element (Current_Ask)
        and then Has_Element (Current_Bid)
      loop
         if Element (Current_Ask).Price > Element (Current_Bid).Price then
            declare
               New_Asks, New_Bids : Offers_At_Price_Lists.List;
            begin
               Create_New_Offers
                 (Ask_Offers => List.Asks (Current_Ask),
                  Bid_Offers => List.Bids (Current_Bid),
                  New_Asks   => New_Asks,
                  New_Bids   => New_Bids);
               for New_Bid of New_Bids loop
                  Next (Current_Bid);
                  List.Bids.Insert (Current_Bid, New_Bid);
               end loop;
               for New_Ask of New_Asks loop
                  List.Asks.Insert (Current_Ask, New_Ask);
               end loop;
               Previous (Current_Ask);
               exit when not Has_Element (Current_Bid)
                 or else not Has_Element (Current_Ask)
                 or else Element (Current_Ask).Price
                 > Element (Current_Bid).Price;
            end;
         end if;

         Execute_Offers_At_Price
           (List.Asks (Current_Ask),
            List.Bids (Current_Bid));

         if List.Asks (Current_Ask).Total = Zero then
            Next (Current_Ask);
         end if;

         if List.Bids (Current_Bid).Total = Zero then
            Previous (Current_Bid);
         end if;

      end loop;

      List.Total_Traded := Total_Traded;
      List.Total_Value := Total_Value;

   end Execute_Transactions;

end Concorde.Transactions;
