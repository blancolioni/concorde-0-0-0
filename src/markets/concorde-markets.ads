private with Memor.Database;
private with Memor.Element_Vectors;

with Concorde.Objects;

with Concorde.Money;
with Concorde.Quantities;

limited with Concorde.Agents;
with Concorde.Trades;

with Concorde.Commodities;
with Concorde.Transactions;

private with Concorde.Trades.Offer_Maps;

package Concorde.Markets is

   type Market_Interface is limited interface
     and Concorde.Objects.Identifier_Object_Interface
     and Concorde.Objects.Named_Object_Interface;

   function Base_Price
     (Market : Market_Interface;
      Item   : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type
      is abstract;

   function Current_Price
     (Market : Market_Interface;
      Item   : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type
      is abstract;

   function Current_Demand
     (Market : Market_Interface;
      Item   : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
      is abstract;

   function Current_Supply
     (Market : Market_Interface;
      Item   : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
      is abstract;

   function Current_Imports
     (Market : Market_Interface;
      Item   : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
      is abstract;

   function Current_Exports
     (Market : Market_Interface;
      Item   : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
      is abstract;

   function Current_Quantity
     (Market : Market_Interface;
      Item   : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
      is abstract;

   function Tax_Rate
     (Market   : Market_Interface;
      Category : Concorde.Trades.Market_Tax_Category;
      Item     : Concorde.Commodities.Commodity_Type)
      return Unit_Real
      is abstract;

   procedure Tax_Receipt
     (Market    : in out Market_Interface;
      Category  : Concorde.Trades.Market_Tax_Category;
      Commodity : Concorde.Commodities.Commodity_Type;
      Tax       : Concorde.Money.Money_Type)
   is abstract;

   procedure Update_Commodity
     (Market        : in out Market_Interface;
      Item          : Concorde.Commodities.Commodity_Type;
      Demand        : Concorde.Quantities.Quantity_Type;
      Supply        : Concorde.Quantities.Quantity_Type;
      Available     : Concorde.Quantities.Quantity_Type;
      Base_Price    : Concorde.Money.Price_Type;
      Current_Price : Concorde.Money.Price_Type)
   is abstract;

   procedure Scan_Agents
     (Market : Market_Interface;
      Process : not null access
        procedure (Agent : not null access constant
                     Concorde.Agents.Root_Agent_Type'Class))
   is abstract;

   type Root_Market_Type is
     new Concorde.Objects.Root_Object_Type
     and Trades.Trade_Interface with private;

   overriding function Current_Price
     (Market    : Root_Market_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type;

   overriding function Historical_Mean_Price
     (Market    : Root_Market_Type;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Concorde.Money.Price_Type;

   overriding function Create_Offer
     (Market    : Root_Market_Type;
      Offer     : Concorde.Trades.Offer_Type;
      Trader    : not null access constant
        Concorde.Trades.Trader_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type)
      return Concorde.Trades.Offer_Reference;

   overriding procedure Delete_Offer
     (Market    : Root_Market_Type;
      Reference : Concorde.Trades.Offer_Reference);

   function Daily_Transaction_Count
     (Market    : Root_Market_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Natural;

   procedure Enable_Logging
     (Market  : in out Root_Market_Type'Class;
      Enabled : Boolean := True);

   function Name
     (Market : Root_Market_Type'Class)
      return String;

   type Market_Type is access constant Root_Market_Type'Class;

   function Create_Market
     (Identifier : String;
      Owner      : not null access constant Market_Interface'Class;
      Manager : not null access constant
        Concorde.Trades.Trade_Manager_Interface'Class;
      Enable_Logging : Boolean)
      return Market_Type;

   procedure Initial_Price
     (Market    : in out Root_Market_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Price     : Concorde.Money.Price_Type);

   type Updateable_Reference (Item : not null access Root_Market_Type'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Root_Market_Type'Class)
      return Updateable_Reference;

private

   type Quantity_Metric_Array is
     array (Concorde.Trades.Quantity_Metric)
     of Concorde.Quantities.Quantity_Type;

   type Market_Offer_Reference is
      record
         Commodity : Concorde.Commodities.Commodity_Type;
         Reference : Concorde.Transactions.Offer_Reference;
      end record;

   type Cached_Commodity_Record is
      record
         Transactions          : Concorde.Transactions
           .Transaction_Request_List;
         Historical_Mean_Price : Concorde.Money.Price_Type :=
                                   Concorde.Money.Zero;
      end record;

   type Cached_Commodity is access Cached_Commodity_Record;

   package Cached_Commodity_Vectors is
     new Memor.Element_Vectors
       (Concorde.Commodities.Root_Commodity_Type,
        Cached_Commodity, null);

   package Market_Offer_Maps is
     new Concorde.Trades.Offer_Maps (Market_Offer_Reference);

   type Root_Market_Type is
     new Concorde.Objects.Root_Object_Type
     and Trades.Trade_Interface with
      record
         Id             : access String;
         Owner          : access constant Market_Interface'Class;
         Manager        : access constant
           Concorde.Trades.Trade_Manager_Interface'Class;
         Enable_Logging : Boolean;
         Commodities    : access Cached_Commodity_Vectors.Vector;
         Offers         : Market_Offer_Maps.Offer_Map;
      end record;

   overriding function Object_Database
     (Market : Root_Market_Type)
      return Memor.Memor_Database;

   overriding function Manager
     (Market : Root_Market_Type)
      return access constant Concorde.Trades.Trade_Manager_Interface'Class
   is (Market.Manager);

   overriding function Current_Quantity
     (Market    : Root_Market_Type;
      Metric    : Concorde.Trades.Quantity_Metric;
      Item      : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Concorde.Quantities.Quantity_Type;

--     overriding procedure Add_Quantity
--       (Market    : Root_Market_Type;
--        Metric    : Concorde.Trades.Quantity_Metric;
--        Item      : not null access constant
--          Concorde.Commodities.Root_Commodity_Type'Class;
--        Quantity  : Concorde.Quantities.Quantity_Type);

   overriding procedure Notify_Foreign_Trade
     (Market    : Root_Market_Type;
      Offer     : Concorde.Trades.Offer_Type;
      Trader    : not null access constant
        Concorde.Trades.Trader_Interface'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type);

   overriding procedure Update_Offer_Price
     (Market    : Root_Market_Type;
      Reference : Concorde.Trades.Offer_Reference;
      New_Price : Concorde.Money.Price_Type);

   overriding procedure Update_Offer_Quantity
     (Market       : Root_Market_Type;
      Reference    : Concorde.Trades.Offer_Reference;
      New_Quantity : Concorde.Quantities.Quantity_Type);

   overriding function Price
     (Market    : Root_Market_Type;
      Offer     : Concorde.Trades.Offer_Type;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Price_Type;

   procedure Check_Commodity
     (Market    : Root_Market_Type'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class);

   function Get_Commodity
     (Market : Root_Market_Type'Class;
      Commodity : not null access constant
        Commodities.Root_Commodity_Type'Class)
      return Cached_Commodity;

   package Db is
     new Memor.Database
       ("market", Root_Market_Type, Market_Type);

   type Updateable_Reference
     (Item : not null access Root_Market_Type'Class)
   is
      record
         Update : Db.Updateable_Reference (Item);
      end record;

end Concorde.Markets;
