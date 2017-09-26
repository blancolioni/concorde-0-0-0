package Concorde.Factions.Create is

   function New_Faction
     (Name                : String;
      Capital             : String;
      Colour              : Lui.Colours.Colour_Type;
      Default_Ship_Design : String)
      return Faction_Type;

end Concorde.Factions.Create;
