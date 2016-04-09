with Concorde.Combat.Ship_Combat;
with Concorde.Systems;
with Concorde.Ships.Lists;

package Concorde.Ships.Battles is

   function Empires_Present
     (Ships : Concorde.Ships.Lists.List)
      return Concorde.Empires.Array_Of_Empires;

   function Empire_Ship_Count
     (Empire : Concorde.Empires.Empire_Type;
      Ships  : Concorde.Ships.Lists.List)
      return Natural;

   function Has_Conflict
     (Ships : Concorde.Ships.Lists.List)
      return Boolean;

   function Create_Arena
     (System : in out Concorde.Systems.Root_Star_System_Type'Class;
      Ships  : Concorde.Ships.Lists.List)
     return Concorde.Combat.Ship_Combat.Space_Combat_Arena;

end Concorde.Ships.Battles;
