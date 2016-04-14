package body Concorde.Markets is

   ------------------------
   -- Calculate_Quantity --
   ------------------------

   function Calculate_Quantity
     (Agreed_Price : Concorde.Money.Price_Type;
      Buy_Or_Sell  : Offer_Type;
      Offer        : Offer_Info)
      return Concorde.Quantities.Quantity
   is
      use Concorde.Money;
      use Concorde.Quantities;
      Factor : Unit_Real;
   begin
      case Buy_Or_Sell is
         when Buy =>
            if Agreed_Price > Offer.Limit_Price then
               Factor := 0.0;
            elsif Agreed_Price > Offer.Offer_Price then
               Factor := (To_Real (Offer.Limit_Price) - To_Real (Agreed_Price))
                 / (To_Real (Offer.Limit_Price) - To_Real (Offer.Offer_Price));
            else
               Factor := 1.0;
            end if;
         when Sell =>
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

   function Create_Market return Market_Type is
   begin
      return new Root_Market_Type'
        (Commodities => new Cached_Commodity_Vectors.Vector);
   end Create_Market;

   ------------------
   -- Create_Offer --
   ------------------

   procedure Create_Offer
     (Market    : in out Root_Market_Type'Class;
      Offer     : Offer_Type;
      Agent     : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity;
      Price     : Concorde.Money.Price_Type;
      Limit     : Concorde.Money.Price_Type)
   is
      Info : constant Cached_Commodity :=
               Market.Get_Commodity (Commodity);
   begin
      case Offer is
         when Buy =>
            Info.Offers.Add_Buy_Offer (Agent, Quantity, Price, Limit);
         when Sell =>
            Info.Offers.Add_Sell_Offer (Agent, Quantity, Price, Limit);
      end case;
   end Create_Offer;

   --------------------
   -- Current_Demand --
   --------------------

   function Current_Demand
     (Market   : Root_Market_Type'Class;
      Item     : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity
   is
   begin
      return Market.Get_Commodity (Item).Offers.Total_Demand;
   end Current_Demand;

   -------------------
   -- Current_Price --
   -------------------

   function Current_Price
     (Market    : Root_Market_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type
   is
   begin
      return Market.Get_Commodity (Commodity).Current_Price;
   end Current_Price;

   --------------------
   -- Current_Supply --
   --------------------

   function Current_Supply
     (Market   : Root_Market_Type'Class;
      Item     : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity
   is
   begin
      return Market.Get_Commodity (Item).Offers.Total_Supply;
   end Current_Supply;

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

   function Historical_Mean_Price
     (Market    : Root_Market_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type
   is
   begin
      return Market.Get_Commodity (Commodity).Historical_Mean_Price;
   end Historical_Mean_Price;

   -----------------
   -- Last_Demand --
   -----------------

   function Last_Demand
     (Market   : Root_Market_Type'Class;
      Item     : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity
   is
   begin
      return Market.Get_Commodity (Item).Demand;
   end Last_Demand;

   -----------------
   -- Last_Supply --
   -----------------

   function Last_Supply
     (Market   : Root_Market_Type'Class;
      Item     : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity
   is
   begin
      return Market.Get_Commodity (Item).Supply;
   end Last_Supply;

   ----------------------
   -- Commodity_Offers --
   ----------------------

   protected body Commodity_Offers is

      -------------------
      -- Add_Buy_Offer --
      -------------------

      procedure Add_Buy_Offer
        (Agent     : not null access constant
           Concorde.Agents.Root_Agent_Type'Class;
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
           Concorde.Agents.Root_Agent_Type'Class;
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
