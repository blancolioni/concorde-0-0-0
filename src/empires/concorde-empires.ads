private with Ada.Containers.Vectors;
private with Concorde.Protected_Lists;

limited with Concorde.AI;

with Lui.Colours;

with Concorde.Objects;
with Concorde.Systems;

package Concorde.Empires is

   type Root_Empire_Type is
     new Concorde.Objects.Root_Named_Object_Type with private;

   function Colour
     (Empire : Root_Empire_Type'Class)
      return Lui.Colours.Colour_Type;

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

   procedure Set_Internal
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Internal : Boolean);
   --  Internal: this system is has connections only to other systems
   --  owned by Empire

   procedure Set_Frontier
     (Empire   : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Frontier : Boolean);
   --  Frontier: this system has at least one connection to an unowned system

   procedure Set_Border
     (Empire  : in out Root_Empire_Type'Class;
      System   : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Border  : Boolean);
   --  Border: this system has at least one connection to a system
   --  owned by another empire

   type Empire_Type is access all Root_Empire_Type'Class;

   function Empire_Count return Natural;
   function Get (Index : Positive) return Empire_Type;

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

   type Empire_Star_System_Record is
      record
         Focus     : Boolean := False;
         Internal  : Boolean := False;
         Frontier  : Boolean := False;
         Border    : Boolean := False;
      end record
     with Pack;

   type Focus_Array is
     array (Positive range <>) of Empire_Star_System_Record
     with Pack;

   type Root_Empire_Type is
     new Concorde.Objects.Root_Named_Object_Type with
      record
         Colour          : Lui.Colours.Colour_Type;
         Focus_List      : access List_Of_Focus_Systems.List;
         System_Data     : access Focus_Array;
         AI              : access Concorde.AI.Root_AI_Type'Class;
         Max_Ships       : Natural := 0;
         Current_Ships   : Natural := 0;
         Current_Systems : Natural := 0;
         Capital         : Concorde.Systems.Star_System_Type;
      end record;

   package Empire_Vectors is
     new Ada.Containers.Vectors (Positive, Empire_Type);

   Vector : Empire_Vectors.Vector;

   procedure Add_Empire (Empire : Empire_Type);

end Concorde.Empires;
