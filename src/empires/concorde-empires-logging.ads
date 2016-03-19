package Concorde.Empires.Logging is

   procedure Start_Logging;
   procedure Stop_Logging;

   procedure Log
     (Empire  : Empire_Type;
      Message : String);

end Concorde.Empires.Logging;
