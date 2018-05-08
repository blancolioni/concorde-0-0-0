with Concorde.Factions;
with Concorde.Worlds;

package Concorde.Ships.Vessels.Create is

   function Create_Start_Vessel
     (Owner       : Concorde.Factions.Faction_Type;
      Name        : String;
      World       : Concorde.Worlds.World_Type;
      Design_Name : String;
      Suffix      : Natural := 0)
      return Ship_Type;

end Concorde.Ships.Vessels.Create;
