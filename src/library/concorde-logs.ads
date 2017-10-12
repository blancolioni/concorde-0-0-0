package Concorde.Logs is

   procedure Log_Line
     (Log_Path : String;
      Line     : String);

   procedure Flush_Logs;

end Concorde.Logs;
