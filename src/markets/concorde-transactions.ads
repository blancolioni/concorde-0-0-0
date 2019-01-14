private with Ada.Containers.Doubly_Linked_Lists;

limited with Concorde.Agents;
with Concorde.Calendar;
with Concorde.Commodities;
with Concorde.Trades;
with Concorde.Money;
with Concorde.Quantities;

package Concorde.Transactions is

   type Offer_Reference is private;

   type Transaction_Request_List is tagged private;

   procedure Create
     (List      : in out Transaction_Request_List'Class;
      Commodity : Concorde.Commodities.Commodity_Type);

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

   function First_Bid_Price
     (List : Transaction_Request_List'Class)
      return Concorde.Money.Price_Type
     with Pre => List.Has_Asks;

   function First_Ask_Quantity
     (List : Transaction_Request_List'Class)
      return Concorde.Quantities.Quantity_Type
     with Pre => List.Has_Asks;

   function First_Bid_Quantity
     (List : Transaction_Request_List'Class)
      return Concorde.Quantities.Quantity_Type
     with Pre => List.Has_Bids;

   function Add_Offer
     (List            : in out Transaction_Request_List'Class;
      Agent           : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Offer           : Concorde.Trades.Offer_Type;
      Quantity        : Concorde.Quantities.Quantity_Type;
      Price           : Concorde.Money.Price_Type;
      Tax             : Unit_Real;
      Tax_Category    : Concorde.Trades.Market_Tax_Category)
      return Offer_Reference;

   procedure Update_Offer_Price
     (List      : in out Transaction_Request_List'Class;
      Reference : Offer_Reference;
      New_Price : Concorde.Money.Price_Type);

   procedure Update_Offer_Quantity
     (List         : in out Transaction_Request_List'Class;
      Reference    : Offer_Reference;
      New_Quantity : Concorde.Quantities.Quantity_Type);

   procedure Delete_Offer
     (List      : in out Transaction_Request_List'Class;
      Reference : Offer_Reference);

   function Daily_Metric
     (List   : Transaction_Request_List'Class;
      Metric : Concorde.Trades.Quantity_Metric)
      return Concorde.Quantities.Quantity_Type;

   function Daily_Average_Buy_Price
     (List : Transaction_Request_List'Class)
      return Concorde.Money.Price_Type;

   function Daily_Average_Sell_Price
     (List : Transaction_Request_List'Class)
      return Concorde.Money.Price_Type;

   function Daily_Average_Price
     (List : Transaction_Request_List'Class)
      return Concorde.Money.Price_Type;

   --     function Daily_Value
--       (List   : Transaction_Request_List'Class;
--        Metric : Concorde.Trades.Quantity_Metric)
--        return Concorde.Money.Money_Type;

   function Daily_Tax
     (List     : Transaction_Request_List'Class;
      Category : Concorde.Trades.Market_Tax_Category)
      return Concorde.Money.Money_Type;

   function Daily_Transaction_Count
     (List : Transaction_Request_List'Class)
     return Natural;

   procedure Resolve (List : in out Transaction_Request_List'Class);

private

   type Tax_Array is
     array (Concorde.Trades.Market_Tax_Category)
     of Concorde.Money.Money_Type;

   type Transaction_Record is
      record
         Time_Stamp      : Concorde.Calendar.Time;
         Buyer           : access constant
           Concorde.Agents.Root_Agent_Type'Class;
         Seller          : access constant
           Concorde.Agents.Root_Agent_Type'Class;
         Commodity       : access constant
           Concorde.Commodities.Root_Commodity_Type'Class;
         Quantity        : Concorde.Quantities.Quantity_Type;
         Seller_Earn     : Concorde.Money.Money_Type;
         Buyer_Cost      : Concorde.Money.Money_Type;
         Taxes           : Tax_Array;
      end record;

   package Transaction_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Transaction_Record);

   type Offer_Record is
      record
         Agent        : access constant Concorde.Agents.Root_Agent_Type'Class;
         Time_Stamp   : Concorde.Calendar.Time;
         Offer        : Concorde.Trades.Offer_Type;
         External     : Boolean;
         Quantity     : Concorde.Quantities.Quantity_Type;
         Price        : Concorde.Money.Price_Type;
         Tax          : Unit_Real;
         Tax_Category : Concorde.Trades.Market_Tax_Category;
      end record;

   package Offer_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Offer_Record);

   type Offer_List_Array is
     array (Concorde.Trades.Offer_Type) of Offer_Lists.List;

   type Offer_Reference is
      record
         Offer    : Concorde.Trades.Offer_Type;
         Position : Offer_Lists.Cursor;
      end record;

   type Transaction_Request_List is tagged
      record
         Commodity      : Concorde.Commodities.Commodity_Type;
         Offers         : Offer_List_Array;
         History        : Offer_Lists.List;
         Transactions   : Transaction_Lists.List;
         Current_Supply : Concorde.Quantities.Quantity_Type :=
                            Concorde.Quantities.Zero;
         Current_Demand : Concorde.Quantities.Quantity_Type :=
                            Concorde.Quantities.Zero;
      end record;

   function Has_Offers
     (List  : Transaction_Request_List'Class;
      Offer : Concorde.Trades.Offer_Type)
      return Boolean
   is (not List.Offers (Offer).Is_Empty);

   function First_Offer
     (List  : Transaction_Request_List'Class;
      Offer : Concorde.Trades.Offer_Type)
      return Offer_Record
   is (case Offer is
          when Concorde.Trades.Ask =>
             List.Offers (Offer).First_Element,
          when Concorde.Trades.Bid =>
             List.Offers (Offer).Last_Element);

   function First_Price
     (List  : Transaction_Request_List'Class;
      Offer : Concorde.Trades.Offer_Type)
      return Concorde.Money.Price_Type
   is (List.First_Offer (Offer).Price);

   function First_Quantity
     (List  : Transaction_Request_List'Class;
      Offer : Concorde.Trades.Offer_Type)
      return Concorde.Quantities.Quantity_Type
   is (List.First_Offer (Offer).Quantity);

   function Has_Asks
     (List : Transaction_Request_List'Class)
      return Boolean
   is (List.Has_Offers (Concorde.Trades.Ask));

   function Has_Bids
     (List : Transaction_Request_List'Class)
      return Boolean
   is (List.Has_Offers (Concorde.Trades.Bid));

   function First_Ask_Price
     (List : Transaction_Request_List'Class)
      return Concorde.Money.Price_Type
   is (List.First_Price (Concorde.Trades.Ask));

   function First_Bid_Price
     (List : Transaction_Request_List'Class)
      return Concorde.Money.Price_Type
   is (List.First_Price (Concorde.Trades.Bid));

   function First_Ask_Quantity
     (List : Transaction_Request_List'Class)
      return Concorde.Quantities.Quantity_Type
   is (List.First_Quantity (Concorde.Trades.Ask));

   function First_Bid_Quantity
     (List : Transaction_Request_List'Class)
      return Concorde.Quantities.Quantity_Type
   is (List.First_Quantity (Concorde.Trades.Bid));

end Concorde.Transactions;
