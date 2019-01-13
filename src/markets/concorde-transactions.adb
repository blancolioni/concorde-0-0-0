with Concorde.Agents;

package body Concorde.Transactions is

   ---------------
   -- Add_Offer --
   ---------------

   function Add_Offer
     (List            : in out Transaction_Request_List'Class;
      Agent           : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Offer           : Concorde.Trades.Offer_Type;
      Quantity        : Concorde.Quantities.Quantity_Type;
      Price           : Concorde.Money.Price_Type;
      Tax             : Unit_Real;
      Tax_Category    : Concorde.Trades.Market_Tax_Category)
      return Offer_Reference
   is
      use type Concorde.Money.Price_Type;
      Before : Offer_Lists.Cursor := Offer_Lists.No_Element;
   begin
      for Position in List.Offers (Offer).Iterate loop
         if Offer_Lists.Element (Position).Price > Price then
            Before := Position;
            exit;
         end if;
      end loop;

      declare
         New_Position : Offer_Lists.Cursor;
         Rec          : constant Offer_Record := Offer_Record'
           (Agent        => Agent,
            Time_Stamp   => Concorde.Calendar.Clock,
            External     => not Agent.Market_Resident,
            Offer        => Offer,
            Quantity     => Quantity,
            Price        => Price,
            Tax          => Tax,
            Tax_Category => Tax_Category);

      begin
         List.History.Append (Rec);
         List.Offers (Offer).Insert
           (Before   => Before,
            New_Item => Rec,
            Position => New_Position);
         return (Offer, New_Position);
      end;
   end Add_Offer;

   ------------
   -- Create --
   ------------

   procedure Create
     (List      : in out Transaction_Request_List'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
   is
   begin
      List.Commodity := Commodity;
   end Create;

   -----------------------------
   -- Daily_Average_Buy_Price --
   -----------------------------

   function Daily_Average_Buy_Price
     (List : Transaction_Request_List'Class)
      return Concorde.Money.Price_Type
   is
      use type Concorde.Calendar.Time;
      use Concorde.Money, Concorde.Quantities;
      Stop : constant Concorde.Calendar.Time :=
               Concorde.Calendar.Clock - Concorde.Calendar.Days (1);
      Total : Money_Type := Zero;
      Quantity : Quantity_Type := Zero;
   begin
      for Transaction of reverse List.Transactions loop
         exit when Transaction.Time_Stamp < Stop;
         Total := Total + Transaction.Buyer_Cost;
         Quantity := Quantity + Transaction.Quantity;
      end loop;

      return Price (Total, Quantity);
   end Daily_Average_Buy_Price;

   -------------------------
   -- Daily_Average_Price --
   -------------------------

   function Daily_Average_Price
     (List : Transaction_Request_List'Class)
      return Concorde.Money.Price_Type
   is
      use type Concorde.Calendar.Time;
      use Concorde.Money, Concorde.Quantities;
      Stop : constant Concorde.Calendar.Time :=
               Concorde.Calendar.Clock - Concorde.Calendar.Days (1);
      Total : Money_Type := Zero;
      Quantity : Quantity_Type := Zero;
   begin
      for Transaction of reverse List.Transactions loop
         exit when Transaction.Time_Stamp < Stop;
         Total := Total
           + Adjust (Transaction.Buyer_Cost + Transaction.Seller_Earn, 0.5);
         Quantity := Quantity + Transaction.Quantity;
      end loop;

      return Price (Total, Quantity);
   end Daily_Average_Price;

   ------------------------------
   -- Daily_Average_Sell_Price --
   ------------------------------

   function Daily_Average_Sell_Price
     (List : Transaction_Request_List'Class)
      return Concorde.Money.Price_Type
   is
      use type Concorde.Calendar.Time;
      use Concorde.Money, Concorde.Quantities;
      Stop : constant Concorde.Calendar.Time :=
               Concorde.Calendar.Clock - Concorde.Calendar.Days (1);
      Total : Money_Type := Zero;
      Quantity : Quantity_Type := Zero;
   begin
      for Transaction of reverse List.Transactions loop
         exit when Transaction.Time_Stamp < Stop;
         Total := Total + Transaction.Seller_Earn;
         Quantity := Quantity + Transaction.Quantity;
      end loop;

      return Price (Total, Quantity);
   end Daily_Average_Sell_Price;

   ------------------
   -- Daily_Metric --
   ------------------

   function Daily_Metric
     (List   : Transaction_Request_List'Class;
      Metric : Concorde.Trades.Quantity_Metric)
      return Concorde.Quantities.Quantity_Type
   is
      use type Concorde.Calendar.Time;
      use Concorde.Trades;
      use Concorde.Quantities;
      Stop : constant Concorde.Calendar.Time :=
               Concorde.Calendar.Clock - Concorde.Calendar.Days (1);
      Result : Quantity_Type := Zero;
   begin
      for Offer of reverse List.History loop
         exit when Offer.Time_Stamp < Stop;
         declare
            Include : constant Boolean :=
                        (case Metric is
                            when Supply =>
                              Offer.Offer = Ask and then not Offer.External,
                            when Demand =>
                              Offer.Offer = Bid and then not Offer.External,
                            when Imports =>
                              Offer.Offer = Ask and then Offer.External,
                            when Exports =>
                              Offer.Offer = Bid and then Offer.External);
         begin
            if Include then
               Result := Result + Offer.Quantity;
            end if;
         end;
      end loop;
      return Result;
   end Daily_Metric;

   ---------------
   -- Daily_Tax --
   ---------------

   function Daily_Tax
     (List     : Transaction_Request_List'Class;
      Category : Concorde.Trades.Market_Tax_Category)
      return Concorde.Money.Money_Type
   is
      use type Concorde.Calendar.Time;
      use Concorde.Money;
      Stop : constant Concorde.Calendar.Time :=
               Concorde.Calendar.Clock - Concorde.Calendar.Days (1);
      Total : Money_Type := Zero;
   begin
      for Transaction of reverse List.Transactions loop
         exit when Transaction.Time_Stamp < Stop;
         Total := Total + Transaction.Taxes (Category);
      end loop;

      return Total;
   end Daily_Tax;

   -----------------
   -- Daily_Value --
   -----------------
--
--     function Daily_Value
--       (List   : Transaction_Request_List'Class;
--        Metric : Concorde.Trades.Quantity_Metric)
--        return Concorde.Money.Money_Type
--     is
--        use type Concorde.Calendar.Time;
--        use Concorde.Trades;
--        use Concorde.Quantities;
--        Stop : constant Concorde.Calendar.Time :=
--                 Concorde.Calendar.Clock - Concorde.Calendar.Days (1);
--        Result : Quantity_Type := Zero;
--     begin
--        for Offer of List.History loop
--           exit when Offer.Time_Stamp < Stop;
--           declare
--              Include : constant Boolean :=
--                          (case Metric is
--                              when Supply  =>
--                            Offer.Offer = Ask and then not Offer.External,
--                              when Demand  =>
--                            Offer.Offer = Bid and then not Offer.External,
--                              when Imports =>
--                                Offer.Offer = Ask and then Offer.External,
--                              when Exports =>
--                                Offer.Offer = Bid and then Offer.External);
--           begin
--              if Include then
--                 Result := Result + Offer.Paid_Before_Tax;
--              end if;
--           end;
--        end loop;
--        return Result;
--     end Daily_Value;

   -----------------------------
   -- Daily_Transaction_Count --
   -----------------------------

   function Daily_Transaction_Count
     (List : Transaction_Request_List'Class)
      return Natural
   is
      use type Concorde.Calendar.Time;
      Stop : constant Concorde.Calendar.Time :=
               Concorde.Calendar.Clock - Concorde.Calendar.Days (1);
      Total : Natural := 0;
   begin
      for Transaction of reverse List.Transactions loop
         exit when Transaction.Time_Stamp < Stop;
         Total := Total + 1;
      end loop;

      return Total;
   end Daily_Transaction_Count;

   ------------------
   -- Delete_Offer --
   ------------------

   procedure Delete_Offer
     (List      : in out Transaction_Request_List'Class;
      Reference : Offer_Reference)
   is
      Deleted : Offer_Lists.Cursor := Reference.Position;
   begin
      List.Offers (Reference.Offer).Delete (Deleted);
   end Delete_Offer;

   -------------
   -- Resolve --
   -------------

   procedure Resolve (List : in out Transaction_Request_List'Class) is
      use Offer_Lists;
      Ask_List : Offer_Lists.List renames List.Offers (Concorde.Trades.Ask);
      Bid_List : Offer_Lists.List renames List.Offers (Concorde.Trades.Bid);
      Ask_Position : Cursor := Ask_List.First;
      Bid_Position : Cursor := Bid_List.Last;
   begin
      while Has_Element (Ask_Position)
        and then Has_Element (Bid_Position)
      loop
         declare
            use Concorde.Money, Concorde.Quantities;
            Ask          : Offer_Record renames Ask_List (Ask_Position);
            Bid          : Offer_Record renames Bid_List (Bid_Position);
            Ask_Price    : constant Price_Type := Ask.Price;
            Bid_Price    : constant Price_Type := Bid.Price;
            Quantity     : constant Quantity_Type :=
                             Min (Ask.Quantity, Bid.Quantity);
            Price        : constant Price_Type :=
                             Adjust_Price (Ask_Price + Bid_Price, 0.5);
            Seller_Price : constant Price_Type :=
                             Without_Tax (Price, Ask.Tax);
            Buyer_Price  : constant Price_Type :=
                             Add_Tax (Price, Bid.Tax);
            Taxes        : Tax_Array := (others => Zero);
         begin
            if Quantity > Zero then
               List.Commodity.Log
                 ("ask "
                  & Show (Ask.Quantity)
                  & " @ "
                  & Show (Ask_Price)
                  & "; bid "
                  & Show (Bid.Quantity)
                  & " @ "
                  & Show (Bid_Price));

               exit when Bid_Price < Ask_Price;

               Ask.Quantity := Ask.Quantity - Quantity;
               Bid.Quantity := Bid.Quantity - Quantity;

               Ask.Agent.Variable_Reference.On_Commodity_Sell
                 (Commodity => List.Commodity,
                  Quantity  => Quantity,
                  Price     => Seller_Price);
               Bid.Agent.Variable_Reference.On_Commodity_Buy
                 (Commodity => List.Commodity,
                  Quantity  => Quantity,
                  Price     => Buyer_Price);

               Taxes (Ask.Tax_Category) :=
                 Total (Price - Seller_Price, Quantity);

               Taxes (Bid.Tax_Category) :=
                 Total (Buyer_Price - Price, Quantity);

               List.Commodity.Log
                 ("sold "
                  & Show (Quantity)
                  & " @ "
                  & Show (Price));

               List.Transactions.Append
                 (Transaction_Record'
                    (Time_Stamp      => Concorde.Calendar.Clock,
                     Buyer           => Bid.Agent,
                     Seller          => Ask.Agent,
                     Commodity       => List.Commodity,
                     Quantity        => Quantity,
                     Seller_Earn     => Total (Seller_Price, Quantity),
                     Buyer_Cost      => Total (Buyer_Price, Quantity),
                     Taxes           => Taxes));
            end if;

            if Ask.Quantity = Zero then
               Next (Ask_Position);
            end if;

            if Bid.Quantity = Zero then
               Previous (Bid_Position);
            end if;

         end;

      end loop;
   end Resolve;

   ------------------
   -- Update_Offer --
   ------------------

   procedure Update_Offer_Price
     (List      : in out Transaction_Request_List'Class;
      Reference : Offer_Reference;
      New_Price : Concorde.Money.Price_Type)
   is
      Rec : Offer_Record renames
              List.Offers (Reference.Offer) (Reference.Position);
   begin
      Rec.Price := New_Price;
   end Update_Offer_Price;

   ---------------------------
   -- Update_Offer_Quantity --
   ---------------------------

   procedure Update_Offer_Quantity
     (List         : in out Transaction_Request_List'Class;
      Reference    : Offer_Reference;
      New_Quantity : Concorde.Quantities.Quantity_Type)
   is
      Rec : Offer_Record renames
              List.Offers (Reference.Offer) (Reference.Position);
   begin
      Rec.Quantity := New_Quantity;
   end Update_Offer_Quantity;

end Concorde.Transactions;
