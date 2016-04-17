package Concorde.Systems.Updates is

   procedure Update_System (System : in out Root_Star_System_Type'Class);

   procedure Update_Market (System : Root_Star_System_Type'Class);

   procedure Execute_Trades (System : Root_Star_System_Type'Class);

   procedure Execute_Production (System : Root_Star_System_Type'Class);

end Concorde.Systems.Updates;
