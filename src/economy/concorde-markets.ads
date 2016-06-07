private with Ada.Containers.Vectors;
private with Memor.Element_Vectors;

with Concorde.Objects;

with Concorde.Money;
with Concorde.Quantities;

with Concorde.Trades;

with Concorde.Commodities;

package Concorde.Markets is

   type Root_Market_Type is
     new Concorde.Objects.Root_Object_Type
     and Trades.Trade_Interface with private;

   overriding function Current_Price
     (Market    : Root_Market_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type;

   overriding function Current_Demand
     (Market   : Root_Market_Type;
      Item     : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity;

   overriding function Current_Supply
     (Market   : Root_Market_Type;
      Item     : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity;

   overriding function Current_Local_Demand
     (Market   : Root_Market_Type;
      Item     : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity;

   overriding function Current_Local_Supply
     (Market   : Root_Market_Type;
      Item     : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity;

   overriding function Last_Average_Ask
     (Market    : Root_Market_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type;

   overriding function Last_Average_Bid
     (Market    : Root_Market_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type;

   overriding function Last_Demand
     (Market   : Root_Market_Type;
      Item     : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity;

   overriding function Last_Supply
     (Market   : Root_Market_Type;
      Item     : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity;

   overriding function Current_Import_Demand
     (Market    : Root_Market_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity;

   overriding function Current_Export_Supply
     (Market    : Root_Market_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity;

   overriding function Historical_Mean_Price
     (Market    : Root_Market_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type;

   overriding procedure Create_Offer
     (Market    : in out Root_Market_Type;
      Offer     : Concorde.Trades.Offer_Type;
      Agent     : not null access constant
        Concorde.Trades.Trader_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity;
      Price     : Concorde.Money.Price_Type;
      Limit     : Concorde.Money.Price_Type);

   overriding procedure Add_Export_Supply
     (Market    : in out Root_Market_Type;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity);

   overriding procedure Add_Import_Demand
     (Market    : in out Root_Market_Type;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity);

   procedure Enable_Logging
     (Market  : in out Root_Market_Type'Class;
      Enabled : Boolean := True);

   overriding procedure Log
     (Market  : Root_Market_Type;
      Message : String);

   function Name
     (Market : Root_Market_Type'Class)
      return String;

   procedure Before_Trading
     (Market : in out Root_Market_Type'Class);

   procedure After_Trading
     (Market : in out Root_Market_Type'Class);

   procedure Execute
     (Market  : in out Root_Market_Type'Class;
      Manager : in out Concorde.Trades.Trade_Manager_Interface'Class);

   type Market_Type is access constant Root_Market_Type'Class;

   function Create_Market
     (Owner  : not null access constant
        Concorde.Objects.Named_Object_Interface'Class;
      Manager : not null access constant
        Concorde.Trades.Trade_Manager_Interface'Class;
      Enable_Logging : Boolean)
      return Market_Type;

   procedure Initial_Price
     (Market    : in out Root_Market_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Price     : Concorde.Money.Price_Type);

private

   use type Concorde.Money.Price_Type;

   type Price_Quantity_Function is
     (No_Change,
      Proportional,
      Quadratic);

   type Offer_Info is
      record
         Agent              : access constant
           Concorde.Trades.Trader_Interface'Class;
         Offered_Quantity   : Concorde.Quantities.Quantity;
         Remaining_Quantity : Concorde.Quantities.Quantity;
         Closed_At_Price    : Concorde.Quantities.Quantity;
         Closed_At_Limit    : Concorde.Quantities.Quantity;
         Offer_Price        : Concorde.Money.Price_Type;
         Current_Price      : Concorde.Money.Price_Type;
         Limit_Price        : Concorde.Money.Price_Type;
         Limit_Quantity     : Price_Quantity_Function;
         Total_Cost         : Concorde.Money.Money_Type;
      end record;

   function "<" (Left, Right : Offer_Info) return Boolean
   is (Left.Current_Price < Right.Current_Price);

   package Offer_Vectors is
     new Ada.Containers.Vectors (Positive, Offer_Info);

   package Offer_Sorting is new Offer_Vectors.Generic_Sorting;

   function Make_Offer
     (Agent    : not null access constant
        Concorde.Trades.Trader_Interface'Class;
      Quantity : Concorde.Quantities.Quantity;
      Price    : Concorde.Money.Price_Type;
      Limit    : Concorde.Money.Price_Type)
      return Offer_Info
   is (Agent, Quantity, Quantity, Quantities.Zero, Quantities.Zero,
       Price, Price, Limit, Proportional, Money.Zero);

   function Calculate_Quantity
     (Agreed_Price : Concorde.Money.Price_Type;
      Buy_Or_Sell  : Concorde.Trades.Offer_Type;
      Commodity    : Concorde.Commodities.Commodity_Type;
      Offer        : Offer_Info)
      return Concorde.Quantities.Quantity;

   protected type Commodity_Offers is
      procedure Add_Buy_Offer
        (Agent     : not null access constant
           Concorde.Trades.Trader_Interface'Class;
         Quantity  : Concorde.Quantities.Quantity;
         Price     : Concorde.Money.Price_Type;
         Limit     : Concorde.Money.Price_Type);
      procedure Add_Sell_Offer
        (Agent     : not null access constant
           Concorde.Trades.Trader_Interface'Class;
         Quantity  : Concorde.Quantities.Quantity;
         Price     : Concorde.Money.Price_Type;
         Limit     : Concorde.Money.Price_Type);
      function Total_Demand return Concorde.Quantities.Quantity;
      function Total_Supply return Concorde.Quantities.Quantity;
      function Local_Demand return Concorde.Quantities.Quantity;
      function Local_Supply return Concorde.Quantities.Quantity;
      function Buy_Offers return Offer_Vectors.Vector;
      function Sell_Offers return Offer_Vectors.Vector;
      procedure Clear;
   private
      Buys  : Offer_Vectors.Vector;
      Sells : Offer_Vectors.Vector;
   end Commodity_Offers;

   type Cached_Commodity_Record is
      record
         Current_Price         : Concorde.Money.Price_Type;
         Historical_Mean_Price : Concorde.Money.Price_Type;
         Last_Average_Ask      : Concorde.Money.Price_Type;
         Last_Average_Bid      : Concorde.Money.Price_Type;
         Local_Supply          : Concorde.Quantities.Quantity;
         Local_Demand          : Concorde.Quantities.Quantity;
         Last_Supply           : Concorde.Quantities.Quantity;
         Last_Demand           : Concorde.Quantities.Quantity;
         Export_Supply         : Concorde.Quantities.Quantity;
         Import_Demand         : Concorde.Quantities.Quantity;
         New_Export_Supply     : Concorde.Quantities.Quantity;
         New_Import_Demand     : Concorde.Quantities.Quantity;
         Supply, Demand        : Concorde.Quantities.Quantity :=
                                   Quantities.Zero;
         Traded_Quantity       : Concorde.Quantities.Quantity :=
                                   Quantities.Zero;
         Offers                : access Commodity_Offers;
      end record;

   type Cached_Commodity is access Cached_Commodity_Record;

   package Cached_Commodity_Vectors is
     new Memor.Element_Vectors (Cached_Commodity, null);

   type Root_Market_Type is
     new Concorde.Objects.Root_Object_Type
     and Trades.Trade_Interface with
      record
         Owner          : access constant
           Concorde.Objects.Named_Object_Interface'Class;
         Manager        : access constant
           Concorde.Trades.Trade_Manager_Interface'Class;
         Enable_Logging : Boolean;
         Commodities    : access Cached_Commodity_Vectors.Vector;
      end record;

   overriding function Object_Database
     (Market : Root_Market_Type)
      return Memor.Root_Database_Type'Class;

   overriding procedure Update
     (Market : Root_Market_Type;
      Perform_Update : not null access
        procedure (M : in out Concorde.Trades.Trade_Interface'Class));

   overriding function Manager
     (Market : Root_Market_Type)
      return access constant Concorde.Trades.Trade_Manager_Interface'Class
   is (Market.Manager);

   procedure Check_Commodity
     (Market    : Root_Market_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type);

   function Get_Commodity
     (Market : Root_Market_Type'Class;
      Commodity : Commodities.Commodity_Type)
      return Cached_Commodity;

   procedure Execute_Commodity_Trades
     (Market    : in out Root_Market_Type'Class;
      Manager   : in out Concorde.Trades.Trade_Manager_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type);

   procedure Log_Offer
     (Market    : Root_Market_Type'Class;
      Message   : String;
      Commodity : Concorde.Commodities.Commodity_Type;
      Offer     : Offer_Info);

end Concorde.Markets;
