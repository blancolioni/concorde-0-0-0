with Concorde.People.Communities;
with Concorde.Factions;

package Concorde.Ships.Vessels.Create is

   function Create_Start_Vessel
     (Owner       : Concorde.Factions.Faction_Type;
      Community   : not null access constant
        Concorde.People.Communities.Root_Community_Type'Class;
      Name        : String;
      Design_Name : String;
      Suffix      : Natural := 0)
      return Ship_Type;

end Concorde.Ships.Vessels.Create;
