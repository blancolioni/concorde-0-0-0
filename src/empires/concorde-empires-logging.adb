with Ada.Text_IO;                      use Ada.Text_IO;

package body Concorde.Empires.Logging is

   File   : File_Type;
   Opened : Boolean := False;

   ---------
   -- Log --
   ---------

   procedure Log
     (Empire  : Empire_Type;
      Message : String)
   is
   begin
      if Opened then
         Put_Line (File, Empire.Name & ": " & Message);
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
