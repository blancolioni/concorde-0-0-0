with Concorde.Commodities;
with Concorde.Dates;
with Concorde.Money;
with Concorde.Quantities;

package Concorde.Trades is

   type Trade_Metric is
     (Export_Demand, Import_Supply,
      Total_Traded, Total_Imported, Total_Exported,
      Local_Demand, Local_Supply);

   type Trade_Interface is limited interface;

   function Current_Price
     (Trade     : Trade_Interface;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type
      is abstract;

   function Historical_Mean_Price
     (Trade     : Trade_Interface;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type
      is abstract;

   function Get_Quantity
     (Trade    : Trade_Interface;
      Item     : Concorde.Commodities.Commodity_Type;
      Metric   : Trade_Metric;
      Start    : Concorde.Dates.Date_Type;
      Finish   : Concorde.Dates.Date_Type)
      return Concorde.Quantities.Quantity_Type
      is abstract;

   function Get_Daily_Quantity
     (Trade    : Trade_Interface'Class;
      Item     : Concorde.Commodities.Commodity_Type;
      Metric   : Trade_Metric;
      Days     : Positive := 1)
      return Concorde.Quantities.Quantity_Type;

   procedure Log
     (Trade   : Trade_Interface;
      Message : String)
   is null;

   type Market_Tax_Category is (Sales, Export, Import);

   type Trade_Manager_Interface is limited interface;

   function Short_Name
     (Manager : Trade_Manager_Interface)
      return String
      is abstract;

   function Tax_Rate
     (Manager   : Trade_Manager_Interface;
      Category  : Market_Tax_Category;
      Commodity : Concorde.Commodities.Commodity_Type)
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

   type Offer_Type is (Ask, Bid);

   type Offer_Price_Strategy is
     (Belief_Based, Fixed_Price, Average_Price);

   type Trader_Interface is limited interface;

   function Short_Name
     (Trader : Trader_Interface)
      return String
      is abstract;

   function Offer_Strategy
     (Trader    : Trader_Interface;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Offer_Price_Strategy
      is abstract;

   function Belief_Based_Strategy
     (Trader : Trader_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Boolean
   is (Trader.Offer_Strategy (Commodity) = Belief_Based);

   function Market_Resident
     (Trader : Trader_Interface)
      return Boolean
      is abstract;
   --  Return True if the trader is a resident of the area serviced by
   --  this market.  Return False if the trader is external (e.g. a ship)

   procedure Create_Offer
     (Trade     : Trade_Interface;
      Offer     : Offer_Type;
      Trader    : not null access constant Trader_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
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

   function Get_Daily_Quantity
     (Trade    : Trade_Interface'Class;
      Item     : Concorde.Commodities.Commodity_Type;
      Metric   : Trade_Metric;
      Days     : Positive := 1)
      return Concorde.Quantities.Quantity_Type
   is (Trade.Get_Quantity
       (Item, Metric,
        Concorde.Dates.Add_Days
          (Concorde.Dates.Current_Date, -Days),
        Concorde.Dates.Current_Date));

end Concorde.Trades;
