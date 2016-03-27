private with Ada.Containers.Vectors;

with Concorde.Empires;
with Concorde.Ships;
with Concorde.Systems;

package Concorde.AI is

   type Root_AI_Type is abstract tagged private;

   procedure Start
     (AI     : in out Root_AI_Type;
      Empire : Concorde.Empires.Empire_Type)
   is abstract;

   procedure Update_Focus
     (AI : in out Root_AI_Type)
   is abstract;

   procedure System_Acquired
     (AI           : in out Root_AI_Type;
      System       : Concorde.Systems.Star_System_Type;
      Former_Owner : Concorde.Empires.Empire_Type)
   is abstract;

   procedure System_Lost
     (AI        : in out Root_AI_Type;
      System    : Concorde.Systems.Star_System_Type;
      New_Owner : Concorde.Empires.Empire_Type)
   is abstract;

   function Minimum_Attack_Factor
     (AI : Root_AI_Type)
      return Non_Negative_Real
   is (1.0);

   function Minimum_Defense_Factor
     (AI : Root_AI_Type)
      return Non_Negative_Real
   is (1.0);

   procedure Allocate_Ships
     (AI : in out Root_AI_Type)
   is abstract;

   procedure Order_Ship
     (AI : in out Root_AI_Type;
      Ship : not null access Concorde.Ships.Root_Ship_Type'Class)
   is abstract;

   function Awake (AI : Root_AI_Type) return Boolean;
   procedure Wake (AI : in out Root_AI_Type);

   type AI_Type is access all Root_AI_Type'Class;

private

   type Destination_Info is
      record
         System     : Concorde.Systems.Star_System_Type;
      end record;

   package Destination_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Concorde.Systems.Star_System_Type,
        "="          => Concorde.Systems."=");

   type Root_AI_Type is abstract tagged
      record
         Empire                  : Concorde.Empires.Empire_Type;
         Current_Attack_Factor   : Non_Negative_Real := 1.2;
         Current_Defense_Factor  : Non_Negative_Real := 1.0;
         Owned_Systems           : Natural := 0;
         Owned_Ships             : Natural := 0;
         Threat_Systems          : Natural := 0;
         Border_Systems          : Natural := 0;
         Frontier_Systems        : Natural := 0;
         Enemy_Strength          : Natural := 0;
         Unexplored_Systems      : Natural := 0;
         Internal_Systems        : Natural := 0;
         Nominal_Defense_Ships   : Natural := 0;
         Defense_Ships           : Natural := 0;
         Exploration_Ships       : Natural := 0;
         Nominal_Reinforce_Ships : Natural := 0;
         Reinforce_Ships         : Natural := 0;
         Offense_Ships           : Natural := 0;
         Battle_Systems          : Natural := 0;
         Defense_Destinations    : Destination_Vectors.Vector;
         Explore_Destinations    : Destination_Vectors.Vector;
         Attack_Destinations     : Destination_Vectors.Vector;
         Battle_Destinations     : Destination_Vectors.Vector;
         Awake                   : Boolean := True;
         Planned_Offensive       : Boolean := False;
         Launch_Offensive        : Boolean := False;
         Required_Strength       : Natural := 0;
         Available_Strength      : Natural := 0;
         Local_Strength          : Natural := 0;
         Attack_From             : Concorde.Systems.Star_System_Type;
         Target                  : Concorde.Systems.Star_System_Type;
      end record;

   procedure Update_Attack_Factor
     (AI           : in out Root_AI_Type'Class;
      Enemy        : Concorde.Empires.Empire_Type;
      Can_Increase : Boolean := True;
      Can_Decrease : Boolean := True);

end Concorde.AI;
