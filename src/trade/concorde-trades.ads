with Concorde.Commodities;
with Concorde.Calendar;
with WL.Money;
with WL.Quantities;

package Concorde.Trades is

   type Trade_Metric is
     (Export_Demand, Import_Supply,
      Total_Traded, Total_Imported, Total_Exported,
      Local_Demand, Local_Supply);

   function Metric_Id (Metric : Trade_Metric) return String;

   type Offer_Type is (Ask, Bid);

   type Offer_Price_Strategy is
     (Belief_Based, Fixed_Price, Average_Price);

   type Trade_Interface is limited interface;

   function Current_Price
     (Trade     : Trade_Interface;
      Commodity : Concorde.Commodities.Commodity_Type)
      return WL.Money.Price_Type
      is abstract;

   function Price
     (Trade     : Trade_Interface;
      Offer     : Offer_Type;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      Quantity  : WL.Quantities.Quantity_Type)
      return WL.Money.Price_Type
      is abstract;

   function Historical_Mean_Price
     (Trade     : Trade_Interface;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return WL.Money.Price_Type
      is abstract;

   function Get_Quantity
     (Trade    : Trade_Interface;
      Item     : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      Metric   : Trade_Metric;
      Start    : Concorde.Calendar.Time;
      Finish   : Concorde.Calendar.Time)
      return WL.Quantities.Quantity_Type
      is abstract;

   function Get_Daily_Quantity
     (Trade    : Trade_Interface'Class;
      Item     : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      Metric   : Trade_Metric;
      Days     : Positive := 1)
      return WL.Quantities.Quantity_Type;

   type Market_Tax_Category is (Sales, Export, Import);

   type Trade_Manager_Interface is limited interface;

   function Short_Name
     (Manager : Trade_Manager_Interface)
      return String
      is abstract;

   function Tax_Rate
     (Manager   : Trade_Manager_Interface;
      Category  : Market_Tax_Category;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Unit_Real
      is abstract;

   procedure Tax_Receipt
     (Manager   : Trade_Manager_Interface;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : WL.Quantities.Quantity_Type;
      Price     : WL.Money.Price_Type;
      Category  : Market_Tax_Category;
      Receipt   : WL.Money.Money_Type)
   is abstract;

   function Manager
     (Trade : Trade_Interface)
      return access constant Trade_Manager_Interface'Class
      is abstract;

   type Trader_Interface is limited interface;

   function Short_Name
     (Trader : Trader_Interface)
      return String
      is abstract;

   function Offer_Strategy
     (Trader    : Trader_Interface;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Offer_Price_Strategy
      is abstract;

   function Belief_Based_Strategy
     (Trader : Trader_Interface'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Boolean
   is (Trader.Offer_Strategy (Commodity) = Belief_Based);

   function Market_Resident
     (Trader : Trader_Interface)
      return Boolean
      is abstract;
   --  Return True if the trader is a resident of the area serviced by
   --  this market.  Return False if the trader is external (e.g. a ship)

   function Available_Capacity
     (Trader : Trader_Interface)
      return WL.Quantities.Quantity_Type
      is abstract;

   procedure Create_Offer
     (Trade     : Trade_Interface;
      Offer     : Offer_Type;
      Trader    : not null access constant Trader_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : WL.Quantities.Quantity_Type;
      Price     : WL.Money.Price_Type)
   is abstract
     with Pre'Class => WL.Money.">" (Price, WL.Money.Zero)
     and then WL.Quantities.">" (Quantity, WL.Quantities.Zero)
     and then Concorde.Commodities."/=" (Commodity, null);

   procedure Delete_Offer
     (Trade     : Trade_Interface;
      Offer     : Offer_Type;
      Trader    : Trader_Interface'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
   is abstract;

   procedure Update_Offer
     (Trade     : Trade_Interface;
      Offer     : Offer_Type;
      Trader    : Trader_Interface'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      New_Price : WL.Money.Price_Type)
   is abstract;

   procedure Execute_Trade
     (Trader    : not null access constant Trader_Interface;
      Offer     : Offer_Type;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : WL.Quantities.Quantity_Type;
      Cost      : WL.Money.Money_Type)
   is abstract;

   procedure Execute_Hire
     (Employer  : not null access constant Trader_Interface;
      Employee  : not null access constant Trader_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : WL.Quantities.Quantity_Type;
      Wage      : WL.Money.Price_Type)
   is null;

   procedure Update_Trader
     (Trader : Trader_Interface;
      Update : not null access
        procedure (Trader : not null access Trader_Interface'Class))
   is abstract;

end Concorde.Trades;