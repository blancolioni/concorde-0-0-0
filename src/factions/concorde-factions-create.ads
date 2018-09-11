with Tropos;

package Concorde.Factions.Create is

   function New_Faction
     (Name                : String;
      Capital             : String;
      Color               : Lui.Colors.Color_Type;
      Default_Ship_Design : String;
      Template            : Tropos.Configuration)
      return Faction_Type;

end Concorde.Factions.Create;
