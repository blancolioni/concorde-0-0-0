private with Memor.Element_Vectors;

with Concorde.Money;
with Concorde.Quantities;

with Concorde.Objects;

with Concorde.Commodities;

with Concorde.Trades;

package Concorde.Agents is

   type Agent_Location_Interface is limited interface;

   function Agent_Location_Name
     (Agent_Location : Agent_Location_Interface)
      return String
      is abstract;

   type Root_Agent_Type is
     abstract new Concorde.Objects.Root_Object_Type
     and Concorde.Commodities.Stock_Interface
     and Concorde.Trades.Trader_Interface with private;

   procedure New_Agent
     (Agent          : in out Root_Agent_Type'Class;
      Location       : not null access constant Agent_Location_Interface'Class;
      Stock_Capacity : Concorde.Quantities.Quantity);

   overriding function Offer_Strategy
     (Agent     : Root_Agent_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Trades.Offer_Price_Strategy
   is (Concorde.Trades.Belief_Based);

   overriding function Maximum_Quantity
     (Agent : Root_Agent_Type)
      return Concorde.Quantities.Quantity;

   overriding function Get_Quantity
     (Agent : Root_Agent_Type;
      Item  : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity;

   overriding function Get_Value
     (Agent : Root_Agent_Type;
      Item  : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Money_Type;

   overriding procedure Set_Quantity
     (Agent    : in out Root_Agent_Type;
      Item     : Concorde.Commodities.Commodity_Type;
      Quantity : Concorde.Quantities.Quantity;
      Value    : Concorde.Money.Money_Type);

   overriding procedure Execute_Trade
     (Agent     : Root_Agent_Type;
      Offer     : Concorde.Trades.Offer_Type;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity;
      Cost      : Concorde.Money.Money_Type);

   overriding function Maximum_Offer_Quantity
     (Agent     : Root_Agent_Type;
      Offer     : Concorde.Trades.Offer_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity;

   overriding procedure Update_Price_Belief
     (Agent             : Root_Agent_Type;
      Market            : Concorde.Trades.Trade_Interface'Class;
      Offer             : Concorde.Trades.Offer_Type;
      Commodity         : Concorde.Commodities.Commodity_Type;
      Total_Traded      : Concorde.Quantities.Quantity;
      Total_Supply      : Concorde.Quantities.Quantity;
      Total_Demand      : Concorde.Quantities.Quantity;
      Average_Price     : Concorde.Money.Price_Type;
      Historical_Price  : Concorde.Money.Price_Type;
      Trader_Price      : Concorde.Money.Price_Type;
      Trader_Offered    : Concorde.Quantities.Quantity;
      Trader_Traded     : Concorde.Quantities.Quantity;
      Total_Money       : Concorde.Money.Money_Type);

   overriding function Market_Resident
     (Agent : Root_Agent_Type)
      return Boolean
   is (True);

   function Cash
     (Agent : Root_Agent_Type'Class)
      return Concorde.Money.Money_Type;

   function Limit_Cash
     (Agent : Root_Agent_Type'Class)
      return Concorde.Money.Money_Type;

   procedure Set_Cash
     (Agent  : in out Root_Agent_Type'Class;
      Amount : Concorde.Money.Money_Type);

   procedure Add_Cash
     (Agent : in out Root_Agent_Type'Class;
      Amount : Concorde.Money.Money_Type);

   procedure Remove_Cash
     (Agent  : in out Root_Agent_Type'Class;
      Amount : Concorde.Money.Money_Type);

   procedure Create_Buy_Offer
     (Agent     : not null access constant Root_Agent_Type'Class;
      Market    : in out Concorde.Trades.Trade_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Desired   : Concorde.Quantities.Quantity;
      Minimum   : Concorde.Quantities.Quantity);

   procedure Create_Sell_Offer
     (Agent     : not null access constant Root_Agent_Type'Class;
      Market    : in out Concorde.Trades.Trade_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Available : Concorde.Quantities.Quantity;
      Minimum   : Concorde.Money.Money_Type);

   procedure Add_Trade_Offers
     (Agent  : not null access constant Root_Agent_Type;
      Market : in out Concorde.Trades.Trade_Interface'Class)
   is abstract;

   procedure Before_Market
     (Agent : in out Root_Agent_Type)
   is null;

   procedure After_Market
     (Agent : in out Root_Agent_Type)
   is null;

   procedure Log
     (Agent    : Root_Agent_Type'Class;
      Category : String;
      Message  : String);

   procedure Log_Production
     (Agent   : Root_Agent_Type'Class;
      Message : String);

   procedure Log_Trade
     (Agent   : Root_Agent_Type'Class;
      Message : String);

   procedure Log_Price
     (Agent   : Root_Agent_Type'Class;
      Message : String);

   procedure Enable_Offer_Logging (Enabled : Boolean := True);

   function Location
     (Agent : Root_Agent_Type'Class)
      return access constant Agent_Location_Interface'Class;

   procedure Set_Location
     (Agent    : in out Root_Agent_Type'Class;
      Location : not null access constant Agent_Location_Interface'Class);

   procedure Set_Guarantor
     (Agent     : in out Root_Agent_Type'Class;
      Guarantor : access constant Root_Agent_Type'Class);

private

   type Agent_Price_Belief_Record is
      record
         Low, High : Concorde.Money.Price_Type;
         Strength  : Unit_Real;
      end record;

   type Agent_Price_Belief_Access is access Agent_Price_Belief_Record;

   package Price_Belief_Vectors is
     new Memor.Element_Vectors (Agent_Price_Belief_Access, null);

   type Root_Agent_Type is
     abstract new Concorde.Objects.Root_Object_Type
     and Concorde.Commodities.Stock_Interface
     and Concorde.Trades.Trader_Interface with
      record
         Stock     : Concorde.Commodities.Root_Stock_Type;
         Cash      : Concorde.Money.Money_Type;
         Belief    : access Price_Belief_Vectors.Vector;
         Location  : access constant Agent_Location_Interface'Class;
         Age       : Natural := 0;
         Guarantor : access constant Root_Agent_Type'Class;
      end record;

   function Get_Price_Belief
     (Agent     : Root_Agent_Type'Class;
      Market    : Concorde.Trades.Trade_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Agent_Price_Belief_Record;

   procedure Update_Price_Belief
     (Agent     : Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Belief    :  Agent_Price_Belief_Record);

end Concorde.Agents;
