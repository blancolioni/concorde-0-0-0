with Concorde.Factions;
with Concorde.Locations;

package Concorde.People.Individuals.Create is

   procedure Create_Random_Individual
     (Loyalty  : Concorde.Factions.Faction_Type;
      Location : Concorde.Locations.Object_Location);

   function Create_Family_Member
     (Faction    : Concorde.Factions.Faction_Type;
      Location   : Concorde.Locations.Object_Location)
      return Individual_Type;

end Concorde.People.Individuals.Create;
