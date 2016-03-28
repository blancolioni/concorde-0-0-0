package body Concorde.Components.Weapons is

   type Weapon_Access is access all Root_Weapon_Component'Class;

   ----------------------
   -- Configure_Weapon --
   ----------------------

   function Configure_Weapon
     (Config : Tropos.Configuration)
      return Weapon_Component
   is
      Result : constant Weapon_Access := new Root_Weapon_Component;
      Dmg_Config : constant Tropos.Configuration :=
                     Config.Child ("damage");
   begin
      Result.Configure_Component (Config);
      Result.Class := Weapon;
      Result.Maximum_Range :=
        Real (Float'(Config.Get ("range")));
      Result.Damage := new Ranged_Damage_Array (1 .. Dmg_Config.Child_Count);
      for I in Result.Damage'Range loop
         Result.Damage (I) := Dmg_Config.Get (I);
      end loop;

      return Weapon_Component (Result);
   end Configure_Weapon;

   -------------------
   -- Ranged_Damage --
   -------------------

   function Ranged_Damage
     (Weapon        : Root_Weapon_Component'Class;
      Distance      : Non_Negative_Real;
      Effectiveness : Unit_Real)
      return Natural
   is
   begin
      return Natural
        (Effectiveness * 10.0 * (1.0 - Distance / Weapon.Maximum_Range)) + 1;
   end Ranged_Damage;

end Concorde.Components.Weapons;
