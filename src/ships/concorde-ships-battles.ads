with Concorde.Combat.Ship_Combat;
with Concorde.Systems;
with Concorde.Ships.Lists;

package Concorde.Ships.Battles is

   function Factions_Present
     (Ships : Concorde.Ships.Lists.List)
      return Concorde.Factions.Array_Of_Factions;

   function Faction_Ship_Count
     (Faction : Concorde.Factions.Faction_Type;
      Ships  : Concorde.Ships.Lists.List)
      return Natural;

   function Has_Conflict
     (Ships : Concorde.Ships.Lists.List)
      return Boolean;

   function Create_Arena
     (System : Concorde.Systems.Star_System_Type;
      Ships  : Concorde.Ships.Lists.List)
     return Concorde.Combat.Ship_Combat.Space_Combat_Arena;

end Concorde.Ships.Battles;
