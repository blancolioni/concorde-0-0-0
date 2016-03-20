with Concorde.Systems;
with Concorde.Ships.Lists;

package Concorde.Ships.Battles is

   function Has_Conflict
     (Ships : Concorde.Ships.Lists.List)
      return Boolean;

   procedure Fight
     (System : Concorde.Systems.Star_System_Access;
      Ships  : Concorde.Ships.Lists.List);

end Concorde.Ships.Battles;
