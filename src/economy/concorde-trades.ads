with Concorde.Commodities;
with Concorde.Money;
with Concorde.Quantities;

package Concorde.Trades is

   type Trade_Interface is limited interface;

   function Current_Price
     (Trade     : Trade_Interface;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type
      is abstract;

   function Current_Demand
     (Trade : Trade_Interface;
      Item     : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity
      is abstract;

   function Current_Supply
     (Trade : Trade_Interface;
      Item     : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity
      is abstract;

   function Last_Demand
     (Trade : Trade_Interface;
      Item     : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity
      is abstract;

   function Last_Supply
     (Trade : Trade_Interface;
      Item     : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity
      is abstract;

   function Historical_Mean_Price
     (Trade : Trade_Interface;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type
      is abstract;

   procedure Log
     (Trade   : Trade_Interface;
      Message : String)
   is null;

   type Offer_Type is (Buy, Sell);

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

   procedure Create_Offer
     (Trade     : in out Trade_Interface;
      Offer     : Offer_Type;
      Trader    : not null access constant Trader_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity;
      Price     : Concorde.Money.Price_Type;
      Limit     : Concorde.Money.Price_Type)
   is abstract;

   procedure Execute_Trade
     (Trader    : Trader_Interface;
      Offer     : Offer_Type;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity;
      Cost      : Concorde.Money.Money_Type)
   is abstract;

   function Maximum_Offer_Quantity
     (Trader    : Trader_Interface;
      Offer     : Offer_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity
      is abstract;

   procedure Update_Price_Belief
     (Trader            : Trader_Interface;
      Trade             : Trade_Interface'Class;
      Offer             : Offer_Type;
      Commodity         : Concorde.Commodities.Commodity_Type;
      Total_Traded      : Concorde.Quantities.Quantity;
      Total_Supply      : Concorde.Quantities.Quantity;
      Total_Demand      : Concorde.Quantities.Quantity;
      Average_Price     : Concorde.Money.Price_Type;
      Historical_Price  : Concorde.Money.Price_Type;
      Trader_Price      : Concorde.Money.Price_Type;
      Trader_Offered    : Concorde.Quantities.Quantity;
      Trader_Traded     : Concorde.Quantities.Quantity;
      Total_Money       : Concorde.Money.Money_Type)
   is abstract;

end Concorde.Trades;
