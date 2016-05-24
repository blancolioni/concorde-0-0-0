package Concorde.Worlds.Updates is

   procedure Update_World (World : in out Root_World_Type'Class);

   procedure Update_World_Government
     (World : Root_World_Type'Class);

   procedure Execute_Trades (World : Root_World_Type'Class);

end Concorde.Worlds.Updates;
