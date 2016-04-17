package Concorde.Logging is

   procedure Log
     (Actor    : String;
      Location : String;
      Category : String;
      Message  : String);

   procedure Start_Logging;
   procedure Stop_Logging;

   procedure Start_Update;
   procedure Finish_Update;

end Concorde.Logging;
