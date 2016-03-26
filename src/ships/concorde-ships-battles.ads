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

   procedure Fight
     (System : Concorde.Systems.Star_System_Access;
      Ships  : Concorde.Ships.Lists.List);

end Concorde.Ships.Battles;
