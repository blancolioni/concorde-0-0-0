package Concorde.Factions.Logging is

   procedure Start_Logging;
   procedure Stop_Logging;

   procedure Log
     (Faction  : not null access constant Root_Faction_Type'Class;
      Message : String);

   procedure Log
     (Faction  : Root_Faction_Type'Class;
      Message : String);

   procedure Flush_Log;

end Concorde.Factions.Logging;
