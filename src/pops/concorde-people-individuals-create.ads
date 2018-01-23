with Concorde.Factions;
with Concorde.Locations;

package Concorde.People.Individuals.Create is

   procedure Create_Random_Individual
     (Loyalty       : Concorde.Factions.Faction_Type;
      Location      : Concorde.Locations.Object_Location;
      Date_Of_Birth : Concorde.Calendar.Time);

   function Create_Family_Member
     (Faction       : Concorde.Factions.Faction_Type;
      Location      : Concorde.Locations.Object_Location;
      Date_Of_Birth : Concorde.Calendar.Time)
      return Individual_Type;

   function Create_Family_Tree
     (Faction    : Concorde.Factions.Faction_Type;
      Location   : Concorde.Locations.Object_Location)
      return Individual_Type;

   function Create_Child
     (Parent_1, Parent_2 : not null access constant Root_Individual_Type'Class;
      Location           : Concorde.Locations.Object_Location;
      Date_Of_Birth      : Concorde.Calendar.Time)
      return Individual_Type;

end Concorde.People.Individuals.Create;
