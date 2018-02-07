with Concorde.Locations;

package Concorde.Armies.Create is

   function New_Army
     (Faction : not null access constant
        Concorde.Factions.Root_Faction_Type'Class;
      Location : Concorde.Locations.Object_Location)
      return Army_Type;

end Concorde.Armies.Create;
