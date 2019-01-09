private with Ada.Containers.Doubly_Linked_Lists;
private with WL.String_Maps;

with Concorde.Agents;
with Concorde.Commodities;
with Concorde.Trades;
with Concorde.Money;
with Concorde.Quantities;

package Concorde.Transactions is

   type Transaction_Request_List
     (Commodity : access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
   is tagged limited private;

   procedure Add_Ask
     (List            : in out Transaction_Request_List'Class;
      Agent           : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Quantity        : Concorde.Quantities.Quantity_Type;
      Price           : Concorde.Money.Price_Type;
      Tax             : Unit_Real;
      Tax_Category    : Concorde.Trades.Market_Tax_Category;
      Minimum_Revenue : Concorde.Money.Money_Type);

   procedure Add_Bid
     (List            : in out Transaction_Request_List'Class;
      Agent           : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Quantity        : Concorde.Quantities.Quantity_Type;
      Price           : Concorde.Money.Price_Type;
      Tax             : Unit_Real;
      Tax_Category    : Concorde.Trades.Market_Tax_Category;
      Maximum_Cost    : Concorde.Money.Money_Type);

   function Total_Asks
     (List : Transaction_Request_List'Class)
      return Concorde.Quantities.Quantity_Type;

   function Total_Bids
     (List : Transaction_Request_List'Class)
      return Concorde.Quantities.Quantity_Type;

   function Total_Traded
     (List : Transaction_Request_List'Class)
      return Concorde.Quantities.Quantity_Type;

   function Average_Price
     (List : Transaction_Request_List'Class)
      return Concorde.Money.Price_Type
     with Pre => Concorde.Quantities.">"
       (List.Total_Traded, Concorde.Quantities.Zero);

   function Total_Value
     (List : Transaction_Request_List'Class)
      return Concorde.Money.Money_Type;

   function Total_Tax
     (List     : Transaction_Request_List'Class;
      Category : Concorde.Trades.Market_Tax_Category)
      return Concorde.Money.Money_Type;

   procedure Execute_Transactions
     (List : in out Transaction_Request_List'Class);

private

   type Agent_Record is
      record
         Agent   : access constant Concorde.Agents.Root_Agent_Type'Class;
         Offered : Concorde.Quantities.Quantity_Type;
         Closed  : Concorde.Quantities.Quantity_Type;
         Budget  : Concorde.Money.Money_Type;
         Value   : Concorde.Money.Money_Type;
      end record;

   package Agent_Maps is
     new WL.String_Maps (Agent_Record);

   type Offer_Record is
      record
         Agent        : access constant Concorde.Agents.Root_Agent_Type'Class;
         Quantity     : Concorde.Quantities.Quantity_Type;
         Price        : Concorde.Money.Price_Type;
         Tax          : Unit_Real;
         Tax_Category : Concorde.Trades.Market_Tax_Category;
         Cost         : Concorde.Money.Money_Type;
      end record;

   package Offer_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Offer_Record);

   type Offers_At_Price_Record is
      record
         Price  : Concorde.Money.Price_Type;
         Total  : Concorde.Quantities.Quantity_Type;
         Offers : Offer_Lists.List;
      end record;

   package Offers_At_Price_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Offers_At_Price_Record);

   type Tax_Array is
     array (Concorde.Trades.Market_Tax_Category)
     of Concorde.Money.Money_Type;

   type Transaction_Request_List
     (Commodity : access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
   is tagged limited
      record
         Bids         : Offers_At_Price_Lists.List;
         Asks         : Offers_At_Price_Lists.List;
         Bidders      : Agent_Maps.Map;
         Askers       : Agent_Maps.Map;
         Total_Asks   : Concorde.Quantities.Quantity_Type :=
                          Concorde.Quantities.Zero;
         Total_Bids   : Concorde.Quantities.Quantity_Type :=
                          Concorde.Quantities.Zero;
         Total_Traded : Concorde.Quantities.Quantity_Type :=
                          Concorde.Quantities.Zero;
         Total_Value  : Concorde.Money.Money_Type :=
                          Concorde.Money.Zero;
         Total_Tax    : Tax_Array :=
                          (others => Concorde.Money.Zero);
      end record;

   function Total_Asks
     (List : Transaction_Request_List'Class)
      return Concorde.Quantities.Quantity_Type
   is (List.Total_Asks);

   function Total_Bids
     (List : Transaction_Request_List'Class)
      return Concorde.Quantities.Quantity_Type
   is (List.Total_Bids);

   function Total_Traded
     (List : Transaction_Request_List'Class)
      return Concorde.Quantities.Quantity_Type
   is (List.Total_Traded);

   function Total_Value
     (List : Transaction_Request_List'Class)
      return Concorde.Money.Money_Type
   is (List.Total_Value);

   function Total_Tax
     (List : Transaction_Request_List'Class;
      Category : Concorde.Trades.Market_Tax_Category)
      return Concorde.Money.Money_Type
   is (List.Total_Tax (Category));

   function Average_Price
     (List : Transaction_Request_List'Class)
      return Concorde.Money.Price_Type
   is (Concorde.Money.Price (List.Total_Value, List.Total_Traded));

end Concorde.Transactions;
