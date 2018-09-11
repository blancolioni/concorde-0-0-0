with WL.Quantities;

package Concorde.People.Communities.Create is

   function New_Community
     (World      : not null access constant
        Concorde.Worlds.Root_World_Type'Class;
      Faction    : not null access constant
        Concorde.Factions.Root_Faction_Type'Class;
      Population : WL.Quantities.Quantity_Type;
      Gini       : Unit_Real;
      Initial_Value : not null access
        function (Parameter_Name : String) return Real)
      return Community_Type;

end Concorde.People.Communities.Create;
