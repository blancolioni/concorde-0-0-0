with Concorde.Installations;
with Concorde.Objects;
with Concorde.Factions;
with Concorde.Locations;
with Concorde.Markets;

package Concorde.Ministries.Create is

   function Create_Faction_Ministry
     (Faction : Concorde.Factions.Faction_Type)
      return Ministry_Type;

   procedure Create_Ministry
     (Faction  : Concorde.Factions.Faction_Type;
      Area     : not null access constant
        Concorde.Objects.Root_Object_Type'Class;
      Location : Concorde.Installations.Installation_Type;
      Market   : Concorde.Markets.Market_Type;
      Name     : String;
      Powers   : Concorde.Powers.Power_Set);

end Concorde.Ministries.Create;
