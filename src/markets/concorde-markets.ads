private with Ada.Containers.Doubly_Linked_Lists;

private with WL.Heaps;

private with Memor.Database;
private with Memor.Element_Vectors;

with Concorde.Calendar;
with Concorde.Objects;

with WL.Money;
with WL.Quantities;

limited with Concorde.Agents;
with Concorde.Trades;

with Concorde.Commodities;

package Concorde.Markets is

   type Root_Market_Type is
     new Concorde.Objects.Root_Object_Type
     and Trades.Trade_Interface with private;

   overriding function Current_Price
     (Market    : Root_Market_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return WL.Money.Price_Type;

   overriding function Historical_Mean_Price
     (Market    : Root_Market_Type;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return WL.Money.Price_Type;

   overriding procedure Create_Offer
     (Market    : Root_Market_Type;
      Offer     : Concorde.Trades.Offer_Type;
      Trader    : not null access constant
        Concorde.Trades.Trader_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : WL.Quantities.Quantity_Type;
      Price     : WL.Money.Price_Type);

   overriding procedure Delete_Offer
     (Market    : Root_Market_Type;
      Offer     : Concorde.Trades.Offer_Type;
      Trader    : Concorde.Trades.Trader_Interface'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class);

   procedure Enable_Logging
     (Market  : in out Root_Market_Type'Class;
      Enabled : Boolean := True);

   function Name
     (Market : Root_Market_Type'Class)
      return String;

   function Recent_Transaction_Count
     (Market : Root_Market_Type'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Natural;

   type Market_Type is access constant Root_Market_Type'Class;

   function Create_Market
     (Identifier : String;
      Owner      : not null access constant
        Concorde.Objects.Named_Object_Interface'Class;
      Manager : not null access constant
        Concorde.Trades.Trade_Manager_Interface'Class;
      Enable_Logging : Boolean)
      return Market_Type;

   procedure Initial_Price
     (Market    : in out Root_Market_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Price     : WL.Money.Price_Type);

   type Updateable_Reference (Item : not null access Root_Market_Type'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Root_Market_Type'Class)
      return Updateable_Reference;

private

   use type WL.Money.Price_Type;

   type Price_Quantity_Function is
     (No_Change,
      Proportional,
      Quadratic);

--     type Quantity_Metric_Array is
--       array (Concorde.Trades.Trade_Metric) of WL.Quantities.Quantity_Type;
--
--     type Historical_Quantity_Record is
--        record
--           Date       : Concorde.Calendar.Time;
--           Quantities : Quantity_Metric_Array :=
--                          (others => WL.Quantities.Zero);
--        end record;
--
--     package Quantity_Metric_Lists is
--       new Ada.Containers.Doubly_Linked_Lists (Historical_Quantity_Record);

   type Offer_Info is
      record
         Agent              : access constant
           Concorde.Agents.Root_Agent_Type'Class;
         Offered_Quantity   : WL.Quantities.Quantity_Type;
         Remaining_Quantity : WL.Quantities.Quantity_Type;
         Offer_Price        : WL.Money.Price_Type;
      end record;

   function "<" (Left, Right : Offer_Info) return Boolean
   is (Left.Offer_Price < Right.Offer_Price);

   function ">" (Left, Right : Offer_Info) return Boolean
   is (Left.Offer_Price > Right.Offer_Price);

   package Bid_Queues is
     new WL.Heaps
       (Key_Type     => WL.Money.Price_Type,
        Element_Type => Offer_Info,
        "<"          => "<");

   package Ask_Queues is
     new WL.Heaps
       (Key_Type     => WL.Money.Price_Type,
        Element_Type => Offer_Info,
        "<"          => ">");

   function Make_Offer
     (Agent    : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Quantity : WL.Quantities.Quantity_Type;
      Price    : WL.Money.Price_Type)
      return Offer_Info
   is (Offer_Info'
         (Agent              => Agent,
          Offered_Quantity   => Quantity,
          Remaining_Quantity => Quantity,
          Offer_Price        => Price));

   type Transaction_Record is
      record
         Time_Stamp : Concorde.Calendar.Time;
         Quantity   : WL.Quantities.Quantity_Type;
         Price      : WL.Money.Price_Type;
         Cost       : WL.Money.Money_Type;
      end record;

   package Recent_Transaction_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Transaction_Record);

   type Offer_Record is
      record
         Time_Stamp : Concorde.Calendar.Time;
         Offer      : Concorde.Trades.Offer_Type;
         Resident   : Boolean;
         Quantity   : WL.Quantities.Quantity_Type;
         Price      : WL.Money.Price_Type;
      end record;

   package Recent_Offer_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Offer_Record);

   type Quantity_Metric_Array is
     array (Concorde.Trades.Quantity_Metric) of WL.Quantities.Quantity_Type;

   type Cached_Commodity_Record is
      record
         Historical_Mean_Price : WL.Money.Price_Type :=
                                   WL.Money.Zero;
         Recent_Transactions   : Recent_Transaction_Lists.List;
         Recent_Offers         : Recent_Offer_Lists.List;
         Recent_Time           : Concorde.Calendar.Time;
         Quantity_Metrics      : Quantity_Metric_Array :=
                                   (others => WL.Quantities.Zero);
         Recent_Trade_Value    : WL.Money.Money_Type :=
                                   WL.Money.Zero;
         Recent_Trade_Volume   : WL.Quantities.Quantity_Type :=
                                   WL.Quantities.Zero;
         Current_Demand        : WL.Quantities.Quantity_Type :=
                                   WL.Quantities.Zero;
         Current_Supply        : WL.Quantities.Quantity_Type :=
                                   WL.Quantities.Zero;
         Bids                  : Bid_Queues.Heap;
         Asks                  : Ask_Queues.Heap;
      end record;

   type Cached_Commodity is access Cached_Commodity_Record;

   procedure Add_Commodity_Offer
     (Info     : Cached_Commodity;
      Offer    : Concorde.Trades.Offer_Type;
      Resident : Boolean;
      Quantity : WL.Quantities.Quantity_Type;
      Price    : WL.Money.Price_Type);

   procedure Remove_Commodity_Offer
     (Info     : Cached_Commodity;
      Offer    : Concorde.Trades.Offer_Type;
      Quantity : WL.Quantities.Quantity_Type;
      Price    : WL.Money.Price_Type);

   package Cached_Commodity_Vectors is
     new Memor.Element_Vectors
       (Concorde.Commodities.Root_Commodity_Type,
        Cached_Commodity, null);

   type Root_Market_Type is
     new Concorde.Objects.Root_Object_Type
     and Trades.Trade_Interface with
      record
         Id             : access String;
         Owner          : access constant
           Concorde.Objects.Named_Object_Interface'Class;
         Manager        : access constant
           Concorde.Trades.Trade_Manager_Interface'Class;
         Enable_Logging : Boolean;
         Commodities    : access Cached_Commodity_Vectors.Vector;
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
      return WL.Quantities.Quantity_Type;

   overriding procedure Update_Offer
     (Market    : Root_Market_Type;
      Offer     : Concorde.Trades.Offer_Type;
      Trader    : Concorde.Trades.Trader_Interface'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      New_Price : WL.Money.Price_Type);

   overriding function Price
     (Market    : Root_Market_Type;
      Offer     : Concorde.Trades.Offer_Type;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      Quantity  : WL.Quantities.Quantity_Type)
      return WL.Money.Price_Type;

   procedure Check_Commodity
     (Market    : Root_Market_Type'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class);

   function Get_Commodity
     (Market : Root_Market_Type'Class;
      Commodity : not null access constant
        Commodities.Root_Commodity_Type'Class)
      return Cached_Commodity;

   procedure Execute_Trade
     (Market     : Root_Market_Type'Class;
      Buyer      : not null access constant
        Concorde.Trades.Trader_Interface'Class;
      Seller     : not null access constant
        Concorde.Trades.Trader_Interface'Class;
      Commodity  : Concorde.Commodities.Commodity_Type;
      Quantity   : WL.Quantities.Quantity_Type;
      Price      : WL.Money.Price_Type);

   procedure Check_Trades
     (Market    : Root_Market_Type'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class);

--     procedure Check_Market
--       (Market : Root_Market_Type'Class);

   procedure Log_Offer
     (Market    : Root_Market_Type'Class;
      Message   : String;
      Commodity : Concorde.Commodities.Commodity_Type;
      Offer     : Offer_Info);

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
