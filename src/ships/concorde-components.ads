private with Tropos;

with Concorde.Objects;

package Concorde.Components is

   type Component_Class is
     (Bridge, Flag_Bridge, Crew_Quarters, Engineering,
      Tank, Hold, Fighter_Bay,
      Engine,
      Power_Plant,
      Sensor,
      Weapon,
      Shield_Generator,
      Cloak,
      Point_Defense);

   type Root_Component_Type is
     abstract new Concorde.Objects.Root_Named_Object_Type
   with private;

   function Class
     (Component : Root_Component_Type'Class)
      return Component_Class;

   function Max_Hits
     (Component : Root_Component_Type'Class)
      return Natural;

   function Max_Stored_Energy
     (Component : Root_Component_Type'Class)
      return Natural;

   function Max_Power_Draw
     (Component : Root_Component_Type'Class)
      return Natural;

   function Mass
     (Component : Root_Component_Type'Class)
      return Natural;

   function Size
     (Component : Root_Component_Type'Class)
      return Natural;

   function Explosion_Chance
     (Component    : Root_Component_Type'Class;
      Remaining_HP : Natural)
      return Unit_Real;

   type Component_Type is access constant Root_Component_Type'Class;

private

   type Explode_Chance is array (Positive range <>) of Unit_Real;

   type Root_Component_Type is
     abstract new Concorde.Objects.Root_Named_Object_Type with
      record
         Mass            : Natural;
         Size            : Natural;
         Class           : Component_Class;
         Power_Draw      : Natural;
         Energy_Store    : Natural;
         Fuel_Store      : Natural;
         Heat_Production : Natural;
         Max_Hits        : Natural;
         Explode         : access Explode_Chance;
      end record;

   procedure Configure_Component
     (Component : in out Root_Component_Type'Class;
      Config    : Tropos.Configuration);

end Concorde.Components;
