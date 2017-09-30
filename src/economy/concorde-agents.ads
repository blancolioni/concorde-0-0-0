private with Ada.Containers.Vectors;
private with Ada.Containers.Doubly_Linked_Lists;
private with WL.String_Maps;

private with Memor.Element_Vectors;

with Concorde.Dates;
with Concorde.Money;
with Concorde.Quantities;

with Concorde.Objects;
with Concorde.Locations;

with Concorde.Commodities;

with Concorde.Trades;

package Concorde.Agents is

   type Root_Agent_Type is
     abstract new Concorde.Objects.Root_Object_Type
     and Concorde.Commodities.Stock_Interface
     and Concorde.Locations.Located_Interface
     and Concorde.Trades.Trader_Interface with private;

   function Class_Name (Agent : Root_Agent_Type) return String
                        is abstract;

   procedure New_Agent
     (Agent          : in out Root_Agent_Type'Class;
      Location       : Concorde.Locations.Object_Location;
      Market         : access constant
        Concorde.Trades.Trade_Interface'Class;
      Stock_Capacity : Concorde.Quantities.Quantity_Type);

   overriding function Offer_Strategy
     (Agent     : Root_Agent_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Trades.Offer_Price_Strategy
   is (Concorde.Trades.Belief_Based);

   overriding function Maximum_Quantity
     (Agent : Root_Agent_Type)
      return Concorde.Quantities.Quantity_Type;

   overriding function Get_Quantity
     (Agent : Root_Agent_Type;
      Item  : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type;

   overriding function Get_Value
     (Agent : Root_Agent_Type;
      Item  : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Money_Type;

   overriding procedure Set_Quantity
     (Agent    : in out Root_Agent_Type;
      Item     : Concorde.Commodities.Commodity_Type;
      Quantity : Concorde.Quantities.Quantity_Type;
      Value    : Concorde.Money.Money_Type);

   overriding procedure Execute_Trade
     (Agent     : not null access constant Root_Agent_Type;
      Offer     : Concorde.Trades.Offer_Type;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Cost      : Concorde.Money.Money_Type);

   overriding procedure Update_Trader
     (Agent  : Root_Agent_Type;
      Update : not null access
        procedure (Agent : not null access
                     Concorde.Trades.Trader_Interface'Class));

   overriding procedure Clear_Stock
     (Agent    : in out Root_Agent_Type);

   overriding procedure Scan_Stock
     (Agent    : Root_Agent_Type;
      Process  : not null access
        procedure (Commodity : Concorde.Commodities.Commodity_Type));

   overriding function Market_Resident
     (Agent : Root_Agent_Type)
      return Boolean
   is (True);

   overriding function Current_Location
     (Agent : Root_Agent_Type)
      return Concorde.Locations.Object_Location;

   overriding function Location_At
     (Agent : Root_Agent_Type;
      Time  : Concorde.Dates.Date_Type)
      return Concorde.Locations.Object_Location
   is (Root_Agent_Type'Class (Agent).Current_Location);

   overriding procedure Set_Location
     (Agent    : in out Root_Agent_Type;
      Location : Concorde.Locations.Object_Location);

   function Variable_Reference
     (Agent : not null access constant Root_Agent_Type)
      return access Root_Agent_Type'Class
      is abstract;

   procedure Set_Market
     (Agent  : in out Root_Agent_Type'Class;
      Market : not null access constant
        Concorde.Trades.Trade_Interface'Class);

   procedure Leave_Market
     (Agent  : in out Root_Agent_Type'Class);

   function Has_Market
     (Agent : Root_Agent_Type'Class)
      return Boolean;

   function Market
     (Agent : Root_Agent_Type'Class)
      return access constant Concorde.Trades.Trade_Interface'Class
     with Pre => Agent.Has_Market;

   function Delayed_Trade_Offers
     (Agent : Root_Agent_Type)
      return Boolean
   is (False);

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

   procedure Create_Ask
     (Agent        : not null access constant Root_Agent_Type'Class;
      Commodity    : Concorde.Commodities.Commodity_Type;
      Ask_Quantity : Concorde.Quantities.Quantity_Type);

   procedure Create_Bid
     (Agent        : not null access constant Root_Agent_Type'Class;
      Commodity    : Concorde.Commodities.Commodity_Type;
      Bid_Quantity : Concorde.Quantities.Quantity_Type);

   procedure Check_Offers
     (Agent : in out Root_Agent_Type'Class);

   function Has_Bids
     (Agent : Root_Agent_Type'Class)
      return Boolean;

   function Has_Bid
     (Agent     : Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Boolean;

   function Has_Asks
     (Agent : Root_Agent_Type'Class)
      return Boolean;

   function Has_Ask
     (Agent     : Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Boolean;

   --     procedure Add_Trade_Offers
--       (Agent  : not null access constant Root_Agent_Type)
--     is abstract;

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

   procedure Log_Movement
     (Agent   : Root_Agent_Type'Class;
      Message : String);

   procedure Enable_Offer_Logging (Enabled : Boolean := True);

   procedure Set_Guarantor
     (Agent     : in out Root_Agent_Type'Class;
      Guarantor : access constant Root_Agent_Type'Class);

   type Agent_Type is access constant Root_Agent_Type'Class;

   function Get_Agent_Access (Agent : Root_Agent_Type'Class) return Agent_Type;

   procedure Scan_Agents
     (Process : not null access procedure
        (Agent : Agent_Type));

   procedure Update_Agents
     (Update : not null access procedure
        (Agent : not null access Root_Agent_Type'Class));

   procedure Save_Agent
     (Agent : not null access constant Root_Agent_Type'Class);

   procedure Remove_Agent
     (Agent : not null access constant Root_Agent_Type'Class);

private

   use Concorde.Quantities;

   type Agent_Price_Belief_Record is
      record
         Low, High : Concorde.Money.Price_Type;
         Strength  : Unit_Real;
      end record;

   type Agent_Price_Belief_Access is access Agent_Price_Belief_Record;

   package Price_Belief_Vectors is
     new Memor.Element_Vectors
       (Concorde.Commodities.Root_Commodity_Type,
        Agent_Price_Belief_Access, null);

   type Account_Entry is
      record
         Date       : Concorde.Dates.Date_Type;
         Item       : Concorde.Commodities.Commodity_Type;
         Entry_Type : Concorde.Trades.Offer_Type;
         Quantity   : Concorde.Quantities.Quantity_Type;
         Cost       : Concorde.Money.Money_Type;
         Balance    : Concorde.Money.Money_Type;
      end record;

   package Account_Entry_Vectors is
      new Ada.Containers.Vectors (Positive, Account_Entry);

   type Agent_Offer is
      record
         Price     : Concorde.Money.Price_Type         := Concorde.Money.Zero;
         Quantity  : Concorde.Quantities.Quantity_Type :=
                       Concorde.Quantities.Zero;
         Filled    : Concorde.Quantities.Quantity_Type :=
                       Concorde.Quantities.Zero;
      end record;

   package Agent_Offer_Vectors is
     new Memor.Element_Vectors
       (Concorde.Commodities.Root_Commodity_Type, Agent_Offer, (others => <>));

   type Root_Agent_Type is
     abstract new Concorde.Objects.Root_Object_Type
     and Concorde.Commodities.Stock_Interface
     and Concorde.Locations.Located_Interface
     and Concorde.Trades.Trader_Interface with
      record
         Market       : access Concorde.Trades.Trade_Interface'Class;
         Stock        : Concorde.Commodities.Root_Stock_Type;
         Cash         : Concorde.Money.Money_Type;
         Belief       : access Price_Belief_Vectors.Vector;
         Location     : Concorde.Locations.Object_Location;
         Age          : Natural := 0;
         Guarantor    : access constant Root_Agent_Type'Class;
         Account      : Account_Entry_Vectors.Vector;
         Bids         : Agent_Offer_Vectors.Vector;
         Asks         : Agent_Offer_Vectors.Vector;
         Ask_Quantity : Concorde.Quantities.Quantity_Type :=
                          Concorde.Quantities.Zero;
         Bid_Quantity : Concorde.Quantities.Quantity_Type :=
                          Concorde.Quantities.Zero;
      end record;

   function Get_Price_Belief
     (Agent     : Root_Agent_Type'Class;
      Market    : not null access constant
        Concorde.Trades.Trade_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Agent_Price_Belief_Record;

   procedure Update_Price_Belief
     (Agent     : Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Belief    :  Agent_Price_Belief_Record);

   function Has_Bids
     (Agent : Root_Agent_Type'Class)
      return Boolean
   is (Agent.Bid_Quantity > Zero);

   function Has_Bid
     (Agent     : Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Boolean
   is (Agent.Bids.Element (Commodity).Quantity
       > Agent.Bids.Element (Commodity).Filled);

   function Has_Asks
     (Agent : Root_Agent_Type'Class)
      return Boolean
   is (Agent.Ask_Quantity > Zero);

   function Has_Ask
     (Agent     : Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Boolean
   is (Agent.Asks.Element (Commodity).Quantity
       > Agent.Asks.Element (Commodity).Filled);

   package Agent_Lists is new Ada.Containers.Doubly_Linked_Lists (Agent_Type);

   package Agent_Maps is
     new WL.String_Maps (Agent_Lists.Cursor, Agent_Lists."=");

   Agent_List : Agent_Lists.List;
   Agent_Map  : Agent_Maps.Map;

end Concorde.Agents;
