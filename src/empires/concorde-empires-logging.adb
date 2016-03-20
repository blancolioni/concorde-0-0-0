with Ada.Text_IO;                      use Ada.Text_IO;

with Concorde.Dates;

package body Concorde.Empires.Logging is

   File   : File_Type;
   Opened : Boolean := False;

   ---------
   -- Log --
   ---------

   procedure Log
     (Empire  : not null access constant Root_Empire_Type'Class;
      Message : String)
   is
   begin
      if Opened then
         Put_Line
           (File,
            Concorde.Dates.To_String
              (Concorde.Dates.Current_Date)
            & ": "
            & Empire.Name & ": " & Message);
      end if;
   end Log;

   -------------------
   -- Start_Logging --
   -------------------

   procedure Start_Logging is
   begin
      if not Opened then
         Create (File, Out_File, "empire-log.txt");
         Opened := True;
      end if;
   end Start_Logging;

   ------------------
   -- Stop_Logging --
   ------------------

   procedure Stop_Logging is
   begin
      if Opened then
         Close (File);
         Opened := False;
      end if;
   end Stop_Logging;

end Concorde.Empires.Logging;
