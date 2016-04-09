private with Memor;

with Concorde.Objects;

package Concorde.Components is

   type Component_Class is
     (Bridge, Flag_Bridge, Crew_Quarters, Engineering,
      Tank, Hold, Fighter_Bay,
      Strut,
      Drive,
      Power_Plant,
      Sensor,
      Energy_Weapon, Kinetic_Weapon, Launcher,
      Shield_Generator,
      Cloak,
      Point_Defense);

   subtype Weapon_Class is Component_Class range Energy_Weapon .. Launcher;

   type Root_Component_Type is
     new Concorde.Objects.Root_Named_Object_Type
   with private;

   function Class
     (Component : Root_Component_Type'Class)
      return Component_Class;

   function Explosion_Chance
     (Component    : Root_Component_Type'Class)
      return Unit_Real;

   function Maximum_Stored_Energy
     (Component : Root_Component_Type'Class;
      Volume    : Positive)
      return Non_Negative_Real;

   function Maximum_Power_Draw
     (Component : Root_Component_Type'Class;
      Volume    : Positive)
      return Non_Negative_Real;

   function Mass
     (Component : Root_Component_Type'Class;
      Volume    : Positive)
      return Non_Negative_Real;

   function Effective_Damage
     (Component     : Root_Component_Type'Class;
      Power         : Non_Negative_Real;
      Effectiveness : Unit_Real;
      At_Range      : Non_Negative_Real)
      return Natural;

   type Component_Type is access constant Root_Component_Type'Class;

private

   type Component_Shape is
     (Rectangular_Prism, Hexagonal_Prism, Cylinder,
      Cone, Sphere, Cube, Conical_Frustum);

   type Root_Component_Type is
     new Concorde.Objects.Root_Named_Object_Type with
      record
         Class                   : Component_Class;
         Shape                   : Component_Shape;
         Density                 : Non_Negative_Real;
         Crew                    : Natural;
         Nominal_Max_Output      : Non_Negative_Real;
         Nominal_Charge          : Non_Negative_Real;
         Nominal_Heat_Production : Non_Negative_Real;
         Nominal_Power_Draw      : Non_Negative_Real;
         Nominal_Half_Range      : Non_Negative_Real;
         Output_Size_Power       : Unit_Real;
         Input_Size_Power        : Unit_Real;
         Charge_Size_Power       : Unit_Real;
         Heat_Size_Power         : Unit_Real;
         Throttle_Step           : Unit_Real;
         Size_Throttle_Factor    : Unit_Real;
         Energy_Coefficient      : Non_Negative_Real;
         Energy_From_Fuel        : Boolean;
         Explosion_Chance        : Unit_Real;
         Layout                  : Size_Type := (1, 1, 1);
      end record;

   overriding function Object_Database
     (Component : Root_Component_Type)
      return Memor.Root_Database_Type'Class;

end Concorde.Components;
