package Concorde.Empires.Logging is

   procedure Start_Logging;
   procedure Stop_Logging;

   procedure Log
     (Empire  : not null access constant Root_Empire_Type'Class;
      Message : String);

end Concorde.Empires.Logging;
