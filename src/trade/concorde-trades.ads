with Concorde.Money;
with Concorde.Quantities;

with Concorde.Commodities;
with Concorde.Contracts;

package Concorde.Trades is

   type Offer_Type is (Ask, Bid);

   type Offer_Price_Strategy is
     (Belief_Based, Fixed_Price, Average_Price);

   type Quantity_Metric is
     (Current_Supply, Current_Demand,
      Current_Imports, Current_Exports);

   type Trade_Interface is limited interface;

   function Current_Price
     (Trade     : Trade_Interface;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type
      is abstract
     with Post'Class =>
       Concorde.Money.">" (Current_Price'Result, Concorde.Money.Zero);

   function Current_Quantity
     (Trade     : Trade_Interface;
      Metric    : Quantity_Metric;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Concorde.Quantities.Quantity_Type
      is abstract;

   procedure Add_Quantity
     (Trade     : Trade_Interface;
      Metric    : Quantity_Metric;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      Quantity  : Concorde.Quantities.Quantity_Type)
   is abstract;

   function Price
     (Trade     : Trade_Interface;
      Offer     : Offer_Type;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Price_Type
      is abstract;

   function Historical_Mean_Price
     (Trade     : Trade_Interface;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Concorde.Money.Price_Type
      is abstract;

   function Current_Supply
     (Trade     : Trade_Interface'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Concorde.Quantities.Quantity_Type
   is (Trade.Current_Quantity (Current_Supply, Commodity));

   function Current_Demand
     (Trade     : Trade_Interface'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Concorde.Quantities.Quantity_Type
   is (Trade.Current_Quantity (Current_Demand, Commodity));

   function Current_Imports
     (Trade     : Trade_Interface'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Concorde.Quantities.Quantity_Type
   is (Trade.Current_Quantity (Current_Imports, Commodity));

   function Current_Exports
     (Trade     : Trade_Interface'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Concorde.Quantities.Quantity_Type
   is (Trade.Current_Quantity (Current_Exports, Commodity));

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
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type;
      Category  : Market_Tax_Category;
      Receipt   : Concorde.Money.Money_Type)
   is abstract;

   function Manager
     (Trade : Trade_Interface)
      return access constant Trade_Manager_Interface'Class
      is abstract;

   type Trader_Interface is limited interface
     and Concorde.Contracts.Contractor_Interface;

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
      return Concorde.Quantities.Quantity_Type
      is abstract;

   procedure Create_Offer
     (Trade     : Trade_Interface;
      Offer     : Offer_Type;
      Trader    : not null access constant Trader_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type)
   is abstract
     with Pre'Class => Concorde.Money.">" (Price, Concorde.Money.Zero)
     and then Concorde.Quantities.">" (Quantity, Concorde.Quantities.Zero)
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
      New_Price : Concorde.Money.Price_Type)
   is abstract;

   procedure Notify_Foreign_Trade
     (Trade     : Trade_Interface;
      Offer     : Offer_Type;
      Trader    : not null access constant Trader_Interface'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type)
   is abstract;

   procedure Execute_Trade
     (Trader    : not null access constant Trader_Interface;
      Offer     : Offer_Type;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Cost      : Concorde.Money.Money_Type)
   is abstract;

   procedure Execute_Hire
     (Employer  : not null access constant Trader_Interface;
      Employee  : not null access constant Trader_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Wage      : Concorde.Money.Price_Type)
   is null;

   procedure Update_Trader
     (Trader : Trader_Interface;
      Update : not null access
        procedure (Trader : not null access Trader_Interface'Class))
   is abstract;

end Concorde.Trades;
