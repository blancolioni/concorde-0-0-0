with Concorde.People.Communities;
with Concorde.Laws;
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
        Concorde.Laws.Law_Target_Interface'Class;
      Location : Concorde.People.Communities.Community_Type;
      Market   : Concorde.Markets.Market_Type;
      Name     : String;
      Budget   : WL.Money.Money_Type;
      Powers   : Concorde.Powers.Power_Set);

end Concorde.Ministries.Create;
