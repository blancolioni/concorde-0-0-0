with Tropos;

package Concorde.People.Communities.Create is

   function New_Community
     (World         : not null access constant
        Concorde.Worlds.Root_World_Type'Class;
      Faction       : not null access constant
        Concorde.Factions.Root_Faction_Type'Class;
      Template      : Tropos.Configuration)
      return Community_Type;

end Concorde.People.Communities.Create;
