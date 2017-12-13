private with Ada.Containers.Vectors;
private with Ada.Containers.Doubly_Linked_Lists;
private with WL.String_Maps;

private with Memor.Element_Vectors;

with Concorde.Calendar;
with WL.Money;
with WL.Quantities;

with Concorde.Objects;
with Concorde.Locations;

with Concorde.Contracts;
with Concorde.Commodities;

with Concorde.Trades;

package Concorde.Agents is

   type Agent_Reference is private;

   type Root_Agent_Type is
     abstract new Concorde.Objects.Root_Object_Type
     and Concorde.Commodities.Stock_Interface
     and Concorde.Locations.Located_Interface
     and Concorde.Contracts.Contractor_Interface
     and Concorde.Trades.Trader_Interface with private;

   overriding function Contracted_To_Buy
     (Agent      : Root_Agent_Type;
      Commodity  : Concorde.Commodities.Commodity_Type)
      return WL.Quantities.Quantity_Type;

   overriding function Contracted_Quantity
     (Agent      : Root_Agent_Type)
      return WL.Quantities.Quantity_Type;

   overriding procedure Add_Contract
     (Agent    : in out Root_Agent_Type;
      Contract : Concorde.Contracts.Contract_Type);

   function Class_Name (Agent : Root_Agent_Type) return String
                        is abstract;

   function Reference (Agent : Root_Agent_Type'Class) return Agent_Reference;

   procedure New_Agent
     (Agent          : in out Root_Agent_Type'Class;
      Location       : Concorde.Locations.Object_Location;
      Government     : access constant
        Root_Agent_Type'Class;
      Market         : access constant
        Concorde.Trades.Trade_Interface'Class;
      Stock_Capacity : WL.Quantities.Quantity_Type);

   function Mean_Price_Belief
     (Agent : Root_Agent_Type'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return WL.Money.Price_Type;

   overriding function Offer_Strategy
     (Agent     : Root_Agent_Type;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Concorde.Trades.Offer_Price_Strategy
   is (Concorde.Trades.Belief_Based);

   overriding function Maximum_Quantity
     (Agent : Root_Agent_Type)
      return WL.Quantities.Quantity_Type;

   overriding function Available_Capacity
     (Agent : Root_Agent_Type)
      return WL.Quantities.Quantity_Type;

   overriding function Get_Quantity
     (Agent : Root_Agent_Type;
      Item  : Concorde.Commodities.Commodity_Type)
      return WL.Quantities.Quantity_Type;

   overriding function Get_Value
     (Agent : Root_Agent_Type;
      Item  : Concorde.Commodities.Commodity_Type)
      return WL.Money.Money_Type;

   overriding procedure Set_Quantity
     (Agent    : in out Root_Agent_Type;
      Item     : Concorde.Commodities.Commodity_Type;
      Quantity : WL.Quantities.Quantity_Type;
      Value    : WL.Money.Money_Type);

   overriding procedure Execute_Trade
     (Agent     : not null access constant Root_Agent_Type;
      Offer     : Concorde.Trades.Offer_Type;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : WL.Quantities.Quantity_Type;
      Cost      : WL.Money.Money_Type);

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
      Time  : Concorde.Calendar.Time)
      return Concorde.Locations.Object_Location
   is (Root_Agent_Type'Class (Agent).Current_Location);

   overriding procedure Set_Location
     (Agent    : in out Root_Agent_Type;
      Location : Concorde.Locations.Object_Location);

   function Variable_Reference
     (Agent : not null access constant Root_Agent_Type)
      return access Root_Agent_Type'Class
      is abstract;

   procedure Set_Government
     (Agent      : in out Root_Agent_Type'Class;
      Government : not null access constant Root_Agent_Type'Class);

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
      return WL.Money.Money_Type;

   function Limit_Cash
     (Agent : Root_Agent_Type'Class)
      return WL.Money.Money_Type;

   procedure Set_Cash
     (Agent  : in out Root_Agent_Type'Class;
      Amount : WL.Money.Money_Type);

   procedure Require_Cash
     (Agent  : in out Root_Agent_Type;
      Amount : WL.Money.Money_Type);

   procedure Add_Cash
     (Agent : in out Root_Agent_Type'Class;
      Amount : WL.Money.Money_Type);

   procedure Remove_Cash
     (Agent  : in out Root_Agent_Type'Class;
      Amount : WL.Money.Money_Type);

   function Last_Earnings
     (Agent : Root_Agent_Type'Class)
      return WL.Money.Money_Type;

   function Last_Expenses
     (Agent : Root_Agent_Type'Class)
      return WL.Money.Money_Type;

   procedure Clear_Current_Account
     (Agent : in out Root_Agent_Type'Class);

   function Government
     (Agent : Root_Agent_Type'Class)
      return access constant Root_Agent_Type'Class;

   function Guarantor
     (Agent : Root_Agent_Type'Class)
      return access constant Root_Agent_Type'Class;

   function Create_Ask_Price
     (Agent        : not null access constant Root_Agent_Type'Class;
      Commodity    : Concorde.Commodities.Commodity_Type)
      return WL.Money.Price_Type;

   function Create_Bid_Price
     (Agent        : not null access constant Root_Agent_Type'Class;
      Commodity    : Concorde.Commodities.Commodity_Type)
      return WL.Money.Price_Type;

   procedure Create_Ask
     (Agent        : not null access constant Root_Agent_Type'Class;
      Commodity    : Concorde.Commodities.Commodity_Type;
      Ask_Quantity : WL.Quantities.Quantity_Type;
      Ask_Price    : WL.Money.Price_Type);

   procedure Create_Bid
     (Agent        : not null access constant Root_Agent_Type'Class;
      Commodity    : Concorde.Commodities.Commodity_Type;
      Bid_Quantity : WL.Quantities.Quantity_Type;
      Bid_Price    : WL.Money.Price_Type);

   procedure Create_Ask
     (Agent        : not null access constant Root_Agent_Type'Class;
      Commodity    : Concorde.Commodities.Commodity_Type;
      Ask_Quantity : WL.Quantities.Quantity_Type);

   procedure Create_Bid
     (Agent        : not null access constant Root_Agent_Type'Class;
      Commodity    : Concorde.Commodities.Commodity_Type;
      Bid_Quantity : WL.Quantities.Quantity_Type);

   procedure Check_Offers
     (Agent : in out Root_Agent_Type'Class);

   function Has_Bid
     (Agent     : Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Boolean;

   function Current_Bid_Quantity
     (Agent     : Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return WL.Quantities.Quantity_Type;

   function Current_Bid_Price
     (Agent     : Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return WL.Money.Price_Type
     with Pre => Agent.Has_Bid (Commodity);

   procedure Clear_Filled_Bids
     (Agent     : in out Root_Agent_Type'Class);

   function Has_Ask
     (Agent     : Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Boolean;

   function Current_Ask_Quantity
     (Agent     : Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return WL.Quantities.Quantity_Type;

   function Current_Ask_Price
     (Agent     : Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return WL.Money.Price_Type
     with Pre => Agent.Has_Ask (Commodity);

   procedure Clear_Filled_Asks
     (Agent     : in out Root_Agent_Type'Class);

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

   procedure Log_Transaction
     (Buyer    : Root_Agent_Type'Class;
      Seller   : not null access constant Root_Agent_Type'Class;
      Item     : Concorde.Commodities.Commodity_Type;
      Quantity : WL.Quantities.Quantity_Type;
      Price    : WL.Money.Price_Type);

   procedure Log_Wages
     (Employer : Root_Agent_Type'Class;
      Worker   : not null access constant Root_Agent_Type'Class;
      Quantity : WL.Quantities.Quantity_Type;
      Price    : WL.Money.Price_Type);

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

   use WL.Quantities;

   type Agent_Reference is new Natural;

   type Agent_Price_Belief_Record is
      record
         Low, High : WL.Money.Price_Type;
         Strength  : Unit_Real;
      end record;

   procedure Translate
     (Belief : in out Agent_Price_Belief_Record;
      Toward : WL.Money.Price_Type;
      Factor : Unit_Real);

   procedure Contract
     (Belief : in out Agent_Price_Belief_Record;
      Factor : Unit_Real);

   procedure Expand
     (Belief : in out Agent_Price_Belief_Record;
      Factor : Unit_Real);

   type Agent_Price_Belief_Access is access Agent_Price_Belief_Record;

   package Price_Belief_Vectors is
     new Memor.Element_Vectors
       (Concorde.Commodities.Root_Commodity_Type,
        Agent_Price_Belief_Access, null);

   type Account_Entry is
      record
         Date       : Concorde.Calendar.Time;
         Item       : Concorde.Commodities.Commodity_Type;
         Entry_Type : Concorde.Trades.Offer_Type;
         Quantity   : WL.Quantities.Quantity_Type;
         Cost       : WL.Money.Money_Type;
         Balance    : WL.Money.Money_Type;
      end record;

   package Account_Entry_Vectors is
      new Ada.Containers.Vectors (Positive, Account_Entry);

   type Agent_Offer is
      record
         Valid     : Boolean := False;
         Price     : WL.Money.Price_Type := WL.Money.Zero;
         Quantity  : WL.Quantities.Quantity_Type :=
                       WL.Quantities.Zero;
         Filled    : WL.Quantities.Quantity_Type :=
                       WL.Quantities.Zero;
      end record;

   procedure Check (Offer : Agent_Offer);

   package Agent_Offer_Vectors is
     new Memor.Element_Vectors
       (Concorde.Commodities.Root_Commodity_Type, Agent_Offer, (others => <>));

   package Current_Contract_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Concorde.Contracts.Contract_Type,
        Concorde.Contracts."=");

   type Root_Agent_Type is
     abstract new Concorde.Objects.Root_Object_Type
     and Concorde.Commodities.Stock_Interface
     and Concorde.Locations.Located_Interface
     and Concorde.Contracts.Contractor_Interface
     and Concorde.Trades.Trader_Interface with
      record
         Agent_Ref             : Agent_Reference;
         Market                : access Concorde.Trades.Trade_Interface'Class;
         Stock                 : Concorde.Commodities.Root_Stock_Type;
         Cash                  : WL.Money.Money_Type;
         Belief                : access Price_Belief_Vectors.Vector;
         Location              : Concorde.Locations.Object_Location;
         Age                   : Natural := 0;
         Guarantor             : access constant Root_Agent_Type'Class;
         Government            : access constant Root_Agent_Type'Class;
         Account               : Account_Entry_Vectors.Vector;
         Bids                  : Agent_Offer_Vectors.Vector;
         Asks                  : Agent_Offer_Vectors.Vector;
         Last_Earnings         : WL.Money.Money_Type := WL.Money.Zero;
         Last_Expenses         : WL.Money.Money_Type := WL.Money.Zero;
         Offered_Contracts     : Current_Contract_Lists.List;
         Accepted_Contracts    : Current_Contract_Lists.List;
         Contracted_Quantities : Concorde.Commodities.Root_Stock_Type;
      end record;

   overriding function Contracted_Quantity
     (Agent      : Root_Agent_Type)
      return WL.Quantities.Quantity_Type
   is (Agent.Contracted_Quantities.Total_Quantity);

   overriding function Contracted_To_Buy
     (Agent      : Root_Agent_Type;
      Commodity  : Concorde.Commodities.Commodity_Type)
      return WL.Quantities.Quantity_Type
   is (Agent.Contracted_Quantities.Get_Quantity (Commodity));

   overriding function Available_Capacity
     (Agent : Root_Agent_Type)
      return WL.Quantities.Quantity_Type
   is (Root_Agent_Type'Class (Agent).Available_Quantity);

   function Reference (Agent : Root_Agent_Type'Class) return Agent_Reference
   is (Agent.Agent_Ref);

   function Get_Price_Belief
     (Agent     : Root_Agent_Type'Class;
      Market    : not null access constant
        Concorde.Trades.Trade_Interface'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Agent_Price_Belief_Record;

   procedure Update_Price_Belief
     (Agent     : Root_Agent_Type'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      Belief    :  Agent_Price_Belief_Record);

   function Has_Bid
     (Agent     : Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Boolean
   is (Agent.Bids.Element (Commodity).Valid);

   function Current_Bid_Price
     (Agent     : Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return WL.Money.Price_Type
   is (Agent.Bids.Element (Commodity).Price);

   function Has_Ask
     (Agent     : Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Boolean
   is (Agent.Asks.Element (Commodity).Valid);

   function Current_Ask_Price
     (Agent     : Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return WL.Money.Price_Type
   is (Agent.Asks.Element (Commodity).Price);

   function Guarantor
     (Agent : Root_Agent_Type'Class)
      return access constant Root_Agent_Type'Class
   is (Agent.Guarantor);

   function Government
     (Agent : Root_Agent_Type'Class)
      return access constant Root_Agent_Type'Class
   is (Agent.Government);

   function Last_Earnings
     (Agent : Root_Agent_Type'Class)
      return WL.Money.Money_Type
   is (Agent.Last_Earnings);

   function Last_Expenses
     (Agent : Root_Agent_Type'Class)
      return WL.Money.Money_Type
   is (Agent.Last_Expenses);

   package Agent_Lists is new Ada.Containers.Doubly_Linked_Lists (Agent_Type);

   package Agent_Maps is
     new WL.String_Maps (Agent_Lists.Cursor, Agent_Lists."=");

   Agent_List : Agent_Lists.List;
   Agent_Map  : Agent_Maps.Map;

end Concorde.Agents;
