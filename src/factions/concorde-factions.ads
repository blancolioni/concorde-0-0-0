private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

private with Concorde.Money;
private with Concorde.Quantities;

--  private with Concorde.Protected_Lists;
private with Memor.Database;
private with Memor.Element_Vectors;

with Memor;

limited with Concorde.People.Communities;
limited with Concorde.People.Individuals;
limited with Concorde.Worlds;

with Lui.Colors;

with Concorde.Agents;
with Concorde.Bureaucracy;
with Concorde.Calendar;
with Concorde.Laws;
with Concorde.Locations;
with Concorde.Managers;
with Concorde.Ministries;
with Concorde.Objects;
with Concorde.Ownership;
with Concorde.Powers;
with Concorde.Systems;
with Concorde.Trades;

package Concorde.Factions is

   Minimum_Relationship : constant := -100;
   Maximum_Relationship : constant := 100;

   type Faction_Relationship_Range is
   range Minimum_Relationship .. Maximum_Relationship;

   type Root_Faction_Type is
     new Concorde.Agents.Root_Agent_Type
     and Memor.Identifier_Record_Type
     and Concorde.Objects.User_Named_Object_Interface
     and Concorde.Ownership.Owner_Interface
     and Concorde.Bureaucracy.Bureaucratic_Interface
     and Concorde.Laws.Law_Target_Interface
     and Concorde.Managers.Managed_Interface
   with private;

   overriding function Director
     (Faction : Root_Faction_Type)
      return access constant
     Concorde.People.Individuals.Root_Individual_Type'Class;

   overriding function Has_Power
     (Faction : Root_Faction_Type;
      Power   : Concorde.Powers.Power_Type)
      return Boolean;

   overriding function Has_Delegated_Power
     (Faction : Root_Faction_Type;
      Power   : Concorde.Powers.Power_Type)
      return Boolean;

   overriding function Delegated_To
     (Faction : Root_Faction_Type;
      Power   : Concorde.Powers.Power_Type)
      return not null access constant
     Concorde.Bureaucracy.Bureaucratic_Interface'Class;

   overriding procedure Add_Power
     (Item  : in out Root_Faction_Type;
      Power : Concorde.Powers.Power_Type);

   overriding procedure Remove_Power
     (Item  : in out Root_Faction_Type;
      Power : Concorde.Powers.Power_Type);

   overriding procedure Delegate_Power
     (Faction : in out Root_Faction_Type;
      Power   : Concorde.Powers.Power_Type;
      To      : not null access constant
        Concorde.Bureaucracy.Bureaucratic_Interface'Class);

   overriding procedure Scan_Powers
     (Item  : Root_Faction_Type;
      Process : not null access
        procedure (Power : Concorde.Powers.Power_Type));

   overriding function Check_Powers
     (Faction   : Root_Faction_Type;
      Test      : not null access
        function (Power : Concorde.Powers.Power_Type) return Boolean)
      return Boolean;

   overriding function Manager
     (Faction : Root_Faction_Type)
      return Concorde.Managers.Manager_Type;

   overriding procedure Set_Manager
     (Faction : in out Root_Faction_Type;
      Manager : Concorde.Managers.Manager_Type);

   function Color
     (Faction : Root_Faction_Type'Class)
      return Lui.Colors.Color_Type;

   function Relationship
     (Faction : Root_Faction_Type'Class;
      To     : Root_Faction_Type'Class)
      return Faction_Relationship_Range;

   procedure Set_Relationship
     (Faction : in out Root_Faction_Type'Class;
      To     : Root_Faction_Type'Class;
      Value  : Faction_Relationship_Range);

   procedure Change_Relationship
     (Faction  : in out Root_Faction_Type'Class;
      To      : Root_Faction_Type'Class;
      Change  : Faction_Relationship_Range);

   function Leader
     (Faction : Root_Faction_Type'Class)
      return access constant
     Concorde.People.Individuals.Root_Individual_Type'Class;

   function First_Ministry
     (Faction : Root_Faction_Type'Class)
      return Concorde.Ministries.Ministry_Type;

   procedure Add_Law
     (Faction : in out Root_Faction_Type'Class;
      Law     : not null access constant
        Concorde.Laws.Root_Law_Type'Class);

   procedure Add_Ministry
     (Faction : in out Root_Faction_Type'Class;
      Ministry : not null access constant
        Concorde.Ministries.Root_Ministry_Type'Class);

   procedure Scan_Ministries
     (Faction : Root_Faction_Type'Class;
      Process : not null access
        procedure (Ministry : Concorde.Ministries.Ministry_Type));

   procedure Set_Minister
     (Faction  : in out Root_Faction_Type'Class;
      Ministry : Concorde.Ministries.Ministry_Type;
      Minister : not null access constant
        Concorde.People.Individuals.Root_Individual_Type'Class);

   procedure Set_Leader
     (Faction  : in out Root_Faction_Type'Class;
      Leader   : not null access constant
        Concorde.People.Individuals.Root_Individual_Type'Class);

   function Capital_Community
     (Faction : Root_Faction_Type'Class)
      return access constant
     Concorde.People.Communities.Root_Community_Type'Class;

   function Capital_World
     (Faction : Root_Faction_Type'Class)
      return access constant
     Concorde.Worlds.Root_World_Type'Class;

   function Current_Ships
     (Faction : Root_Faction_Type'Class)
      return Natural;

   procedure New_Ship
     (Faction : in out Root_Faction_Type'Class);

   procedure Remove_Ship
     (Faction : in out Root_Faction_Type'Class);

   procedure Change_Ships
     (Faction : in out Root_Faction_Type'Class;
      Change : Integer);

   function Current_Systems
     (Faction : Root_Faction_Type'Class)
      return Natural;

   function Owned_World
     (Faction : Root_Faction_Type'Class;
      World : not null access constant
        Concorde.Worlds.Root_World_Type'Class)
      return Boolean;

   function Default_Ship_Design
     (Faction : Root_Faction_Type'Class)
      return String;

   type Faction_Type is access constant Root_Faction_Type'Class;

   type Citizen_Interface is limited interface;

   function Citizenship
     (Citizen : Citizen_Interface)
      return Faction_Type
      is abstract;

   function Get_By_Name (Name : String) return Faction_Type;

   function Faction_Count return Natural;

   type Array_Of_Factions is array (Positive range <>) of Faction_Type;

   type Ranking is (Normal, By_Star_Systems, By_Ships);

   function Rank
     (Rank_Type : Ranking)
      return Array_Of_Factions;

   function Get
     (Rank_Type : Ranking;
      Index     : Positive)
      return Faction_Type;

   procedure Check_Invariants;

   --     procedure System_Acquired
   --       (Faction : in out Root_Faction_Type'Class;
   --        System : in out Concorde.Systems.Root_Star_System_Type'Class);
   --
   --     procedure System_Lost
   --       (Faction : in out Root_Faction_Type'Class;
   --        System : in out Concorde.Systems.Root_Star_System_Type'Class)
   --       with Pre => System.Owner.Name = Faction.Name
   --       and then Faction.Current_Systems > 0
   --       and then (Faction.Current_Systems > 1 or else System.Capital);

   type Star_System_Flag is
     (Discovered, Visible, Owned,
      Active_Battle, Claim, Focus,
      Attack_Target, Opportunity_Target,
      Internal, Frontier, Neighbour, Border);

   --  Discovered: we know that this star system exists
   --  Visible: we can currently see what's in this star system
   --  Owned: we own the system
   --  Active_Battle: we are currently fighting a battle in this system
   --  Claim: we have a claim to this system
   --  Focus: we are keeping tabs on this system
   --  Attack_Target: we are planning to attack this system
   --  Opportunity_Target: we will attack this system if weakly garrisoned
   --  Internal: all neighbours of this system are owned by us
   --  Frontier: at least one neighbour of this system is owned by nobody
   --  Neighbour: this system is not owned by us, but a neighbour is
   --  Border: system has at least one neighbour owned by a different Faction

   procedure Clear_System_Flags
     (Faction   : in out Root_Faction_Type'Class;
      System   : Concorde.Systems.Root_Star_System_Type'Class);
   --  Clear all flags apart from Focus

   function Is_Set
     (Faction   : Root_Faction_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Flag     : Star_System_Flag)
      return Boolean;

   function Is_Clear
     (Faction   : Root_Faction_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Flag     : Star_System_Flag)
      return Boolean;

   procedure Set
     (Faction   : in out Root_Faction_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Flag     : Star_System_Flag);

   procedure Set
     (Faction   : in out Root_Faction_Type'Class;
      System   : Concorde.Systems.Root_Star_System_Type'Class;
      Flag     : Star_System_Flag);

   procedure Clear
     (Faction   : in out Root_Faction_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Flag     : Star_System_Flag);

   function Is_Internal
     (Faction   : Root_Faction_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is (Faction.Is_Set (System, Internal));

   function Has_Battle
     (Faction   : Root_Faction_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is (Faction.Is_Set (System, Active_Battle));

   function Has_Claim
     (Faction   : Root_Faction_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is (Faction.Is_Set (System, Claim));

   function Is_Frontier
     (Faction   : Root_Faction_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is (Faction.Is_Set (System, Frontier));

   function Is_Neighbour
     (Faction   : Root_Faction_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is (Faction.Is_Set (System, Neighbour));

   function Is_Border
     (Faction   : Root_Faction_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is (Faction.Is_Set (System, Border));

   function Is_Attack_Target
     (Faction   : Root_Faction_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is (Faction.Is_Set (System, Attack_Target));

   function Is_Opportunity_Target
     (Faction   : Root_Faction_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is (Faction.Is_Set (System, Opportunity_Target));

   function Required
     (Faction : Root_Faction_Type'Class;
      System : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Integer;

   procedure Set_Required
     (Faction   : in out Root_Faction_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Required : Integer);

   procedure Change_Required
     (Faction   : in out Root_Faction_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Change   : Integer);

   function Next_Path_Node_Index
     (Faction   : Root_Faction_Type'Class;
      From, To : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Natural;

   function Path_Length
     (Faction   : Root_Faction_Type'Class;
      From, To : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Natural;

   procedure Clear_Battles;

   procedure Update_System_Owner
     (Owner  : in out Root_Faction_Type'Class;
      System : Concorde.Systems.Root_Star_System_Type'Class)
     with Pre => Owner.Identifier = System.Owner.Identifier;

   procedure Scan_Factions
     (Process : not null access
        procedure (Faction : Faction_Type));

   type Updateable_Reference (Item : not null access Root_Faction_Type'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Root_Faction_Type'Class)
      return Updateable_Reference;

private

   type Star_System_Flag_Values is
     array (Star_System_Flag) of Boolean
     with Pack;

   type Destination_Info is
      record
         Path_Length : Natural := 0;
         Next_Node   : Integer := 0;
      end record;

   --  next_node:
   --  -1: not cached
   --   0: no path
   --   1 .. system count: index of next node in path

   type Destination_Next_Index is
     array (Positive range <>) of Destination_Info
     with Pack;

   type Destination_Next_Access is
     access Destination_Next_Index;

   type Faction_Star_System_Record is
      record
         Flags     : Star_System_Flag_Values := (others => False);
         Required  : Integer := 0;
         Next_Node : Destination_Next_Access := null;
      end record
     with Pack;

   type System_Data_Array is
     array (Positive range <>) of Faction_Star_System_Record
     with Pack;

   package List_Of_Systems is
     new Ada.Containers.Doubly_Linked_Lists
       (Concorde.Systems.Star_System_Type, Concorde.Systems."=");

   procedure Check_Cache (Faction   : Root_Faction_Type'Class;
                          From, To : not null access constant
                            Concorde.Systems.Root_Star_System_Type'Class);

   procedure Clear_Path_Cache (Faction : in out Root_Faction_Type'Class);

   type Faction_Data_Record is
      record
         Relationship : Faction_Relationship_Range := 0;
      end record;

   type Relation_Record;

   type Individual_Access is access constant
     Concorde.People.Individuals.Root_Individual_Type'Class;

   package Ministry_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Concorde.Ministries.Ministry_Type, Concorde.Ministries."=");

   package Law_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Concorde.Laws.Law_Type, Concorde.Laws."=");

   type Root_Faction_Type is
     new Concorde.Agents.Root_Agent_Type
     and Memor.Identifier_Record_Type
     and Concorde.Objects.User_Named_Object_Interface
     and Concorde.Ownership.Owner_Interface
     and Concorde.Bureaucracy.Bureaucratic_Interface
     and Concorde.Laws.Law_Target_Interface
     and Concorde.Managers.Managed_Interface with
      record
         Identifier         : Ada.Strings.Unbounded.Unbounded_String;
         Faction_Name       : Ada.Strings.Unbounded.Unbounded_String;
         Color             : Lui.Colors.Color_Type;
         Central_Bank       : Boolean := False;
         System_Data        : access System_Data_Array;
         Faction_Data       : access Relation_Record;
         Ruler              : Faction_Type;
         Manager            : Concorde.Managers.Manager_Type;
         Laws               : Law_Lists.List;
         Ministries         : Ministry_Lists.List;
         Current_Population : Concorde.Quantities.Quantity_Type;
         Current_Ships      : Natural := 0;
         Current_Units      : Natural := 0;
         Current_Relations  : Natural := 0;
         Current_Systems    : Natural := 0;
         Built_Ships        : Natural := 0;
         Captured_Ships     : Natural := 0;
         Lost_Ships         : Natural := 0;
         Destroyed_Ships    : Natural := 0;
         Border_Change      : Boolean;
         Capital_Community  : access constant
           Concorde.People.Communities.Root_Community_Type'Class;
         Default_Ship       : access String;
      end record;

   overriding procedure Update_Agent
     (Faction        : not null access constant Root_Faction_Type;
      Perform_Update : not null access
        procedure (Agent : in out Concorde.Agents.Root_Agent_Type'Class));

   overriding function Class_Name (Faction : Root_Faction_Type) return String
   is ("Faction");

   overriding function Object_Database
     (Faction : Root_Faction_Type)
      return Memor.Memor_Database;

   overriding function Identifier
     (Faction : Root_Faction_Type)
      return String
   is (Ada.Strings.Unbounded.To_String (Faction.Identifier));

   overriding function Name (Faction : Root_Faction_Type) return String
   is (Ada.Strings.Unbounded.To_String (Faction.Faction_Name));

   overriding function Short_Name (Faction : Root_Faction_Type) return String
   is (Identifier (Faction));

   overriding procedure Set_Name
     (Faction : in out Root_Faction_Type;
      Name   : String);

   overriding function Location_At
     (Faction : Root_Faction_Type;
      Time    : Concorde.Calendar.Time)
      return Concorde.Locations.Object_Location;

   overriding function Variable_Reference
     (Faction : not null access constant Root_Faction_Type)
      return access Concorde.Agents.Root_Agent_Type'Class
   is (Faction.Update.Item);

   overriding function Variable_Reference
     (Faction : not null access constant Root_Faction_Type)
      return access Concorde.Bureaucracy.Bureaucratic_Interface'Class
   is (Faction.Update.Item);

   overriding procedure Require_Cash
     (Faction : in out Root_Faction_Type;
      Amount  : Concorde.Money.Money_Type);

   overriding function Director
     (Faction : Root_Faction_Type)
      return access constant
     Concorde.People.Individuals.Root_Individual_Type'Class
   is (Faction.Ministries.First_Element.Director);

   overriding function Has_Power
     (Faction : Root_Faction_Type;
      Power   : Concorde.Powers.Power_Type)
      return Boolean
   is (for some Ministry of Faction.Ministries =>
          Ministry.Has_Power (Power));

   overriding function Has_Delegated_Power
     (Faction : Root_Faction_Type;
      Power   : Concorde.Powers.Power_Type)
      return Boolean
   is (Faction.Ministries.First_Element.Has_Delegated_Power (Power));

   overriding function Check_Powers
     (Faction : Root_Faction_Type;
      Test      : not null access
        function (Power : Concorde.Powers.Power_Type) return Boolean)
      return Boolean
   is (for some Ministry of Faction.Ministries =>
          Ministry.Check_Powers (Test));

   overriding function Delegated_To
     (Faction : Root_Faction_Type;
      Power   : Concorde.Powers.Power_Type)
      return not null access constant
     Concorde.Bureaucracy.Bureaucratic_Interface'Class
   is (Faction.Ministries.First_Element.Delegated_To (Power));

   overriding function Manager
     (Faction : Root_Faction_Type)
      return Concorde.Managers.Manager_Type
   is (Faction.Manager);

   function First_Ministry
     (Faction : Root_Faction_Type'Class)
      return Concorde.Ministries.Ministry_Type
   is (Faction.Ministries.First_Element);

   function Leader
     (Faction : Root_Faction_Type'Class)
      return access constant
     Concorde.People.Individuals.Root_Individual_Type'Class
   is (Faction.Director);

   package Faction_Vectors is
     new Memor.Element_Vectors
       (Index_Type    => Root_Faction_Type,
        Element_Type  => Faction_Data_Record,
        Default_Value => (Relationship => 0));

   type Relation_Record is
      record
         Vector : Faction_Vectors.Vector;
      end record;

   package Db is
     new Memor.Database
       ("faction", Root_Faction_Type, Faction_Type);

   function Faction_Count return Natural
   is (Db.Active_Count);

   type Updateable_Reference
     (Item : not null access Root_Faction_Type'Class)
   is
      record
         Update : Db.Updateable_Reference (Item);
      end record;

   Imperium_Faction : Faction_Type;

end Concorde.Factions;
