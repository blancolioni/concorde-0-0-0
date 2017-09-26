package Concorde.Ships.Create is

   function New_Ship
     (Owner  : not null access constant
        Concorde.Factions.Root_Faction_Type'Class;
      Name   : String;
      World  : Concorde.Worlds.World_Type;
      Design : String;
      Suffix : Natural := 0)
      return Ship_Type;

end Concorde.Ships.Create;
