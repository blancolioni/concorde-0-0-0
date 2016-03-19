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
     (Empire : in out Root_Empire_Type'Class;
      Focus  : Concorde.Systems.Star_System_Type);

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
      Focus  : Concorde.Systems.Star_System_Type)
      return Boolean;

   function AI
     (Empire : Root_Empire_Type'Class)
      return access Concorde.AI.Root_AI_Type'Class;

   function Maximum_Supported_Fleets
     (Empire : Root_Empire_Type'Class)
      return Natural;

   function Current_Fleets
     (Empire : Root_Empire_Type'Class)
      return Natural;

   function Available_Fleet_Capacity
     (Empire : Root_Empire_Type'Class)
      return Natural;

   procedure New_Fleets
     (Empire : in out Root_Empire_Type'Class;
      Count  : Natural);

   type Empire_Type is access all Root_Empire_Type'Class;

   function Empire_Count return Natural;
   function Get (Index : Positive) return Empire_Type;

   type Ranking is (Normal, By_Star_Systems, By_Fleets);

   function Get
     (Rank_Type : Ranking;
      Index     : Positive)
      return Empire_Type;

private

   package List_Of_Systems is
     new Concorde.Protected_Lists
       (Concorde.Systems.Star_System_Type,
        Concorde.Systems."=");

   type Root_Empire_Type is
     new Concorde.Objects.Root_Named_Object_Type with
      record
         Colour         : Lui.Colours.Colour_Type;
         Focus_List     : access List_Of_Systems.List;
         AI             : access Concorde.AI.Root_AI_Type'Class;
         Max_Fleets     : Natural := 0;
         Current_Fleets : Natural := 0;
         Capital        : Concorde.Systems.Star_System_Type;
      end record;

   package Empire_Vectors is
     new Ada.Containers.Vectors (Positive, Empire_Type);

   Vector : Empire_Vectors.Vector;

   procedure Add_Empire (Empire : Empire_Type);

end Concorde.Empires;
