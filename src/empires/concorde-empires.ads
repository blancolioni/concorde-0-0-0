private with Ada.Containers.Vectors;
private with Concorde.Protected_Lists;

limited with Concorde.AI;

with Lui.Colours;

with Concorde.Objects;
with Concorde.Systems;

package Concorde.Empires is

   Minimum_Relationship : constant := -100;
   Maximum_Relationship : constant := 100;

   type Empire_Relationship_Range is
   range Minimum_Relationship .. Maximum_Relationship;

   type Root_Empire_Type is
     new Concorde.Objects.Root_Named_Object_Type with private;

   function Colour
     (Empire : Root_Empire_Type'Class)
      return Lui.Colours.Colour_Type;

   function Capital
     (Empire : Root_Empire_Type'Class)
      return Concorde.Systems.Star_System_Type;

   procedure Add_Focus
     (Empire   : in out Root_Empire_Type'Class;
      Focus    : Concorde.Systems.Star_System_Type;
      Priority : Non_Negative_Real := 1.0);

   procedure Remove_Focus
     (Empire : in out Root_Empire_Type'Class;
      Focus  : Concorde.Systems.Star_System_Type);

   procedure Remove_Focus
     (Empire : in out Root_Empire_Type'Class;
      Matching : not null access
        function (System : Concorde.Systems.Star_System_Type)
      return Boolean);

   function Has_Focus
     (Empire : Root_Empire_Type'Class;
      Focus  : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean;

   function Minimum_Score_Focus
     (Empire : Root_Empire_Type'Class;
      Score  : not null access
        function (System : Concorde.Systems.Star_System_Type)
      return Natural)
      return Concorde.Systems.Star_System_Type;
   --  focus system which minimised Score is returned

   function Relationship
     (Empire : Root_Empire_Type'Class;
      To     : not null access constant Root_Empire_Type'Class)
      return Empire_Relationship_Range;

   procedure Set_Relationship
     (Empire : in out Root_Empire_Type'Class;
      To     : not null access constant Root_Empire_Type'Class;
      Value  : Empire_Relationship_Range);

   procedure Change_Relationship
     (Empire  : in out Root_Empire_Type'Class;
      To      : not null access constant Root_Empire_Type'Class;
      Change  : Empire_Relationship_Range);

   function AI
     (Empire : Root_Empire_Type'Class)
      return access Concorde.AI.Root_AI_Type'Class;

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

   function Current_Systems
     (Empire : Root_Empire_Type'Class)
      return Natural;

   procedure System_Acquired
     (Empire : in out Root_Empire_Type'Class;
      System : Concorde.Systems.Star_System_Type);

   procedure System_Lost
     (Empire : in out Root_Empire_Type'Class;
      System : Concorde.Systems.Star_System_Type);

   procedure Clear_System_Flags
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class);
   --  Clear all flags apart from Focus

   function Is_Internal
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean;

   procedure Set_Internal
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Internal : Boolean);
   --  Internal: this system is has connections only to other systems
   --  owned by Empire

   function Is_Frontier
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean;

   procedure Set_Frontier
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Frontier : Boolean);
   --  Frontier: this system has at least one connection to an unowned system

   function Is_Neighbour
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean;

   procedure Set_Neighbour
     (Empire    : in out Root_Empire_Type'Class;
      System    : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Neighbour : Boolean);
   --  Neighbour: this system has a neighbour which is owned by Empire

   function Is_Border
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean;

   procedure Set_Border
     (Empire  : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Border  : Boolean);
   --  Border: this system has at least one connection to a system
   --  owned by another empire

   function Is_Attack_Target
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean;

   procedure Set_Attack_Target
     (Empire        : in out Root_Empire_Type'Class;
      System        : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Attack_Target : Boolean);
   --  Attack_Target: Emprie planning an attack on this system

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
     (Empire : in out Root_Empire_Type'Class;
      From, To : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Natural;

   function Path_Length
     (Empire : in out Root_Empire_Type'Class;
      From, To : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Natural;

   function Owned_System
     (Empire : Root_Empire_Type'Class;
      System : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean;

   function Neighbour_System
     (Empire : Root_Empire_Type'Class;
      System : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Boolean;
   --  System is a neighbour of a system which is owned by Empire

   type Empire_Type is access all Root_Empire_Type'Class;

   function Empire_Count return Natural;
   function Get (Index : Positive) return Empire_Type;

   type Array_Of_Empires is array (Positive range <>) of Empire_Type;

   type Ranking is (Normal, By_Star_Systems, By_Ships);

   function Get
     (Rank_Type : Ranking;
      Index     : Positive)
      return Empire_Type;

   procedure Unload;

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

   type Empire_Star_System_Record is
      record
         Focus     : Boolean := False;
         Internal  : Boolean := False;
         Frontier  : Boolean := False;
         Border    : Boolean := False;
         Neighbour : Boolean := False;
         Attack    : Boolean := False;
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

   type Empire_Data_Array is
     array (Positive range <>) of Empire_Data_Record;

   type Root_Empire_Type is
     new Concorde.Objects.Root_Named_Object_Type with
      record
         Index           : Positive;
         Colour          : Lui.Colours.Colour_Type;
         Focus_List      : access List_Of_Focus_Systems.List;
         System_Data     : access System_Data_Array;
         Empire_Data     : access Empire_Data_Array;
         AI              : access Concorde.AI.Root_AI_Type'Class;
         Max_Ships       : Natural := 0;
         Current_Ships   : Natural := 0;
         Current_Systems : Natural := 0;
         Built_Ships     : Natural := 0;
         Lost_Ships      : Natural := 0;
         Capital         : Concorde.Systems.Star_System_Type;
      end record;

   package Empire_Vectors is
     new Ada.Containers.Vectors (Positive, Empire_Type);

   Vector : Empire_Vectors.Vector;

   procedure Add_Empire (Empire : Empire_Type);

   procedure Check_Cache (Empire   : in out Root_Empire_Type'Class;
                          From, To : not null access constant
                            Concorde.Systems.Root_Star_System_Type'Class);

   procedure Clear_Path_Cache (Empire : in out Root_Empire_Type'Class);

end Concorde.Empires;
