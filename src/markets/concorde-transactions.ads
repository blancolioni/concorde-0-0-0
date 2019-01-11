private with Ada.Containers.Doubly_Linked_Lists;

with Concorde.Agents;
with Concorde.Calendar;
with Concorde.Commodities;
with Concorde.Trades;
with Concorde.Money;
with Concorde.Quantities;

package Concorde.Transactions is

   type Transaction_Request_List
     (Commodity : access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
   is tagged private;

   function Has_Asks
     (List : Transaction_Request_List'Class)
      return Boolean;

   function Has_Bids
     (List : Transaction_Request_List'Class)
      return Boolean;

   function First_Ask_Price
     (List : Transaction_Request_List'Class)
      return Concorde.Money.Price_Type
     with Pre => List.Has_Asks;

   function First_Ask_Quantity
     (List : Transaction_Request_List'Class)
      return Concorde.Money.Price_Type
     with Pre => List.Has_Asks;

   function First_Bid_Quantity
     (List : Transaction_Request_List'Class)
      return Concorde.Money.Price_Type
     with Pre => List.Has_Bids;

   procedure Add_Ask
     (List            : in out Transaction_Request_List'Class;
      Agent           : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Quantity        : Concorde.Quantities.Quantity_Type;
      Price           : Concorde.Money.Price_Type;
      Tax             : Unit_Real;
      Tax_Category    : Concorde.Trades.Market_Tax_Category);

   procedure Add_Bid
     (List            : in out Transaction_Request_List'Class;
      Agent           : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Quantity        : Concorde.Quantities.Quantity_Type;
      Price           : Concorde.Money.Price_Type;
      Tax             : Unit_Real;
      Tax_Category    : Concorde.Trades.Market_Tax_Category);

   function Total_Asks
     (List : Transaction_Request_List'Class)
      return Concorde.Quantities.Quantity_Type;

   function Total_Bids
     (List : Transaction_Request_List'Class)
      return Concorde.Quantities.Quantity_Type;

   function Daily_Traded
     (List : Transaction_Request_List'Class)
      return Concorde.Quantities.Quantity_Type;

   function Daily_Average_Price
     (List : Transaction_Request_List'Class)
      return Concorde.Money.Price_Type
     with Pre => Concorde.Quantities.">"
       (List.Total_Asks, Concorde.Quantities.Zero)
     or else
       Concorde.Quantities.">"
         (List.Total_Bids, Concorde.Quantities.Zero);

   function Daily_Value
     (List : Transaction_Request_List'Class)
      return Concorde.Money.Money_Type;

   function Daily_Tax
     (List     : Transaction_Request_List'Class;
      Category : Concorde.Trades.Market_Tax_Category)
      return Concorde.Money.Money_Type;

   procedure Resolve (List : in out Transaction_Request_List'Class);

private

   type Tax_Array is
     array (Concorde.Trades.Market_Tax_Category)
     of Concorde.Money.Money_Type;

   type Transaction_Record is
      record
         Time            : Concorde.Calendar.Time;
         Buyer           : access constant
           Concorde.Agents.Root_Agent_Type'Class;
         Seller          : access constant
           Concorde.Agents.Root_Agent_Type'Class;
         Commodity       : access constant
           Concorde.Commodities.Root_Commodity_Type'Class;
         Quantity        : Concorde.Quantities.Quantity_Type;
         Paid_Before_Tax : Concorde.Money.Money_Type;
         Tax             : Tax_Array;
      end record;

   package Transaction_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Transaction_Record);

   type Offer_Record is
      record
         Agent        : access constant Concorde.Agents.Root_Agent_Type'Class;
         Offer        : Concorde.Trades.Offer_Type;
         Quantity     : Concorde.Quantities.Quantity_Type;
         Price        : Concorde.Money.Price_Type;
         Tax          : Unit_Real;
         Tax_Category : Concorde.Trades.Market_Tax_Category;
      end record;

   package Offer_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Offer_Record);

   type Transaction_Request_List
     (Commodity : access constant
        Concorde.Commodities.Root_Commodity_Type'Class) is tagged
      record
         Bids         : Offer_Lists.List;
         Asks         : Offer_Lists.List;
         Transactions : Transaction_Lists.List;
      end record;

end Concorde.Transactions;
