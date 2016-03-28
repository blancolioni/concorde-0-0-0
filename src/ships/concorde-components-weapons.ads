with Tropos;

package Concorde.Components.Weapons is

   type Root_Weapon_Component is
     new Root_Component_Type with private;

   function Ranged_Damage
     (Weapon        : Root_Weapon_Component'Class;
      Distance      : Non_Negative_Real;
      Effectiveness : Unit_Real)
      return Natural;

   type Weapon_Component is access constant Root_Weapon_Component'Class;

   function Configure_Weapon
     (Config : Tropos.Configuration)
      return Weapon_Component;

private

   type Ranged_Damage_Array is array (Natural range <>) of Natural;

   type Root_Weapon_Component is
     new Root_Component_Type with
      record
         Maximum_Range : Non_Negative_Real;
         Damage        : access Ranged_Damage_Array;
      end record;

end Concorde.Components.Weapons;
