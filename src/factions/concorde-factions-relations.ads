package Concorde.Factions.Relations is

   function Has_Conflict
     (Es : Array_Of_Factions)
      return Boolean;

   function At_War
     (E1, E2 : Root_Faction_Type'Class)
      return Boolean;

end Concorde.Factions.Relations;
