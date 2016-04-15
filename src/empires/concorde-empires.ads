private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

private with Concorde.Protected_Lists;
private with Memor.Element_Vectors;

with Memor;

limited with Concorde.Players;

with Lui.Colours;

with Concorde.Agents;
with Concorde.Trades;
with Concorde.Objects;
with Concorde.Systems;

package Concorde.Empires is

   Minimum_Relationship : constant := -100;
   Maximum_Relationship : constant := 100;

   type Empire_Relationship_Range is
   range Minimum_Relationship .. Maximum_Relationship;

   type Root_Empire_Type is
     new Concorde.Agents.Root_Agent_Type
     and Memor.Identifier_Record_Type
     and Concorde.Objects.Named_Object_Interface
   with private;

   function Colour
     (Empire : Root_Empire_Type'Class)
      return Lui.Colours.Colour_Type;

   function Capital
     (Empire : Root_Empire_Type'Class)
      return Concorde.Systems.Star_System_Type;

   function Relationship
     (Empire : Root_Empire_Type'Class;
      To     : Root_Empire_Type'Class)
      return Empire_Relationship_Range;

   procedure Set_Relationship
     (Empire : in out Root_Empire_Type'Class;
      To     : Root_Empire_Type'Class;
      Value  : Empire_Relationship_Range);

   procedure Change_Relationship
     (Empire  : in out Root_Empire_Type'Class;
      To      : Root_Empire_Type'Class;
      Change  : Empire_Relationship_Range);

   function Player
     (Empire : Root_Empire_Type'Class)
      return access Concorde.Players.Root_Player_Type'Class;

   function Maximum_Supported_Ships
     (Empire : Root_Empire_Type'Class)
      return Natural;

   function Current_Ships
     (Empire : Root_Empire_Type'Class)
      return Natural;

   function Available_Ship_Capacity
     (Empire : Root_Empire_Type'Class)
      return Natural;

   procedure New_Ship
     (Empire : in out Root_Empire_Type'Class);

   procedure Remove_Ship
     (Empire : in out Root_Empire_Type'Class);

   procedure Change_Ships
     (Empire : in out Root_Empire_Type'Class;
      Change : Integer);

   function Current_Systems
     (Empire : Root_Empire_Type'Class)
      return Natural;

   procedure System_Acquired
     (Empire : in out Root_Empire_Type'Class;
      System : in out Concorde.Systems.Root_Star_System_Type'Class);

   procedure System_Lost
     (Empire : in out Root_Empire_Type'Class;
      System : in out Concorde.Systems.Root_Star_System_Type'Class)
     with Pre => System.Owner.Name = Empire.Name
     and then Empire.Current_Systems > 0
     and then (Empire.Current_Systems > 1 or else System.Capital);

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
   --  Border: system has at least one neighbour owned by a different empire

   procedure Clear_System_Flags
     (Empire   : in out Root_Empire_Type'Class;
      System   : Concorde.Systems.Root_Star_System_Type'Class);
   --  Clear all flags apart from Focus

   function Is_Set
     (Empire   : Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Flag     : Star_System_Flag)
      return Boolean;

   function Is_Clear
     (Empire   : Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Flag     : Star_System_Flag)
      return Boolean;

   procedure Set
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Flag     : Star_System_Flag);

   procedure Set
     (Empire   : in out Root_Empire_Type'Class;
      System   : Concorde.Systems.Root_Star_System_Type'Class;
      Flag     : Star_System_Flag);

   procedure Clear
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Flag     : Star_System_Flag);

   function Is_Internal
     (Empire   : Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is (Empire.Is_Set (System, Internal));

   function Has_Battle
     (Empire   : Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is (Empire.Is_Set (System, Active_Battle));

   function Has_Claim
     (Empire   : Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is (Empire.Is_Set (System, Claim));

   function Is_Frontier
     (Empire   : Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is (Empire.Is_Set (System, Frontier));

   function Is_Neighbour
     (Empire   : Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is (Empire.Is_Set (System, Neighbour));

   function Is_Border
     (Empire   : Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is (Empire.Is_Set (System, Border));

   function Is_Attack_Target
     (Empire   : Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is (Empire.Is_Set (System, Attack_Target));

   function Is_Opportunity_Target
     (Empire   : Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean
   is (Empire.Is_Set (System, Opportunity_Target));

   function Required
     (Empire : Root_Empire_Type'Class;
      System : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Integer;

   procedure Set_Required
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Required : Integer);

   procedure Change_Required
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Change   : Integer);

   function Next_Path_Node_Index
     (Empire : Root_Empire_Type'Class;
      From, To : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Natural;

   function Path_Length
     (Empire : Root_Empire_Type'Class;
      From, To : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Natural;

   function Owned_System
     (Empire : Root_Empire_Type'Class;
      System : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean;

   function Default_Ship_Design
     (Empire : Root_Empire_Type'Class)
      return String;

   type Empire_Type is access constant Root_Empire_Type'Class;

   type Array_Of_Empires is array (Positive range <>) of Empire_Type;

   type Ranking is (Normal, By_Star_Systems, By_Ships);

   function Rank
     (Rank_Type : Ranking)
      return Array_Of_Empires;

   function Get
     (Rank_Type : Ranking;
      Index     : Positive)
      return Empire_Type;

   procedure Check_Invariants;

   procedure Clear_Battles;

   procedure Update_System_Owner
     (Owner  : in out Root_Empire_Type'Class;
      System : Concorde.Systems.Root_Star_System_Type'Class)
     with Pre => Owner.Identifier = System.Owner.Identifier;

private

   package List_Of_Focus_Systems is
     new Concorde.Protected_Lists
       (Concorde.Systems.Star_System_Type,
        Concorde.Systems."=");

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

   type Star_System_Flag_Values is
     array (Star_System_Flag) of Boolean
     with Pack;

   type Empire_Star_System_Record is
      record
         Flags     : Star_System_Flag_Values := (others => False);
         Required  : Integer := 0;
         Next_Node : Destination_Next_Access := null;
      end record
     with Pack;

   type System_Data_Array is
     array (Positive range <>) of Empire_Star_System_Record
     with Pack;

   type Empire_Data_Record is
      record
         Relationship : Empire_Relationship_Range := 0;
      end record;

   package Empire_Vectors is
     new Memor.Element_Vectors (Empire_Data_Record, (Relationship => 0));

   package List_Of_Systems is
     new Ada.Containers.Doubly_Linked_Lists
       (Concorde.Systems.Star_System_Type, Concorde.Systems."=");

   type Root_Empire_Type is
     new Concorde.Agents.Root_Agent_Type
     and Memor.Identifier_Record_Type
     and Concorde.Objects.Named_Object_Interface with
      record
         Identifier      : Ada.Strings.Unbounded.Unbounded_String;
         Empire_Name     : Ada.Strings.Unbounded.Unbounded_String;
         Colour          : Lui.Colours.Colour_Type;
         System_Data     : access System_Data_Array;
         Empire_Data     : Empire_Vectors.Vector;
         Player          : access Concorde.Players.Root_Player_Type'Class;
         Max_Ships       : Non_Negative_Real := 0.0;
         Current_Ships   : Natural := 0;
         Current_Systems : Natural := 0;
         Built_Ships     : Natural := 0;
         Captured_Ships  : Natural := 0;
         Lost_Ships      : Natural := 0;
         Destroyed_Ships : Natural := 0;
         Border_Change   : Boolean;
         Capital         : Concorde.Systems.Star_System_Type;
         Battles         : List_Of_Systems.List;
         Default_Ship    : access String;
      end record;

   overriding function Object_Database
     (Empire : Root_Empire_Type)
      return Memor.Root_Database_Type'Class;

   overriding function Identifier
     (Empire : Root_Empire_Type)
      return String
   is (Ada.Strings.Unbounded.To_String (Empire.Identifier));

   overriding function Name (Empire : Root_Empire_Type) return String
   is (Ada.Strings.Unbounded.To_String (Empire.Empire_Name));

   overriding procedure Set_Name
     (Empire : in out Root_Empire_Type;
      Name   : String);

   overriding procedure Add_Trade_Offers
     (Empire : not null access constant Root_Empire_Type;
      Market : in out Concorde.Trades.Trade_Interface'Class)
   is null;

   procedure Check_Cache (Empire   : Root_Empire_Type'Class;
                          From, To : not null access constant
                            Concorde.Systems.Root_Star_System_Type'Class);

   procedure Clear_Path_Cache (Empire : in out Root_Empire_Type'Class);

end Concorde.Empires;
