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

   type Trader_Interface is limited interface;

   procedure Create_Offer
     (Trade     : in out Trade_Interface;
      Offer     : Offer_Type;
      Trader    : not null access constant Trader_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity;
      Price     : Concorde.Money.Price_Type;
      Limit     : Concorde.Money.Price_Type)
   is abstract;

end Concorde.Trades;
