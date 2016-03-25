with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Text_IO;

with Concorde.Dates;
with Concorde.Paths;

package body Concorde.Empires.Logging is

   Started : Boolean := False;

   package List_Of_Log_Lines is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   package Empire_Log_Vectors is
     new Ada.Containers.Vectors (Positive, List_Of_Log_Lines.List,
                                 List_Of_Log_Lines."=");

   Current_Log_Date : Concorde.Dates.Date_Type := 0;
   Current_Logs     : Empire_Log_Vectors.Vector;

   function Log_File_Path
     (Empire : Empire_Type)
      return String
   is (Concorde.Paths.Config_Path & "/../log/empires/"
       & Empire.Name
       & ".txt");

   ---------------
   -- Flush_Log --
   ---------------

   procedure Flush_Log is
   begin
      if not Started then
         return;
      end if;

      for Empire of Vector loop
         if not Current_Logs.Element (Empire.Index).Is_Empty then
            declare
               use Ada.Text_IO;
               File : File_Type;
            begin
               Open (File, Append_File, Log_File_Path (Empire));
               New_Line (File);
               Put_Line (File, "----------------");
               Put_Line (File,
                         "-- "
                         & Concorde.Dates.To_String (Current_Log_Date)
                         & " --");
               Put_Line (File, "----------------");
               New_Line (File);

               for Line of Current_Logs.Element (Empire.Index) loop
                  Put_Line (File, Line);
               end loop;
               Close (File);
               Current_Logs (Empire.Index).Clear;
            end;
         end if;
      end loop;
   end Flush_Log;

   ---------
   -- Log --
   ---------

   procedure Log
     (Empire  : not null access constant Root_Empire_Type'Class;
      Message : String)
   is
   begin
      Log (Empire.all, Message);
   end Log;

   ---------
   -- Log --
   ---------

   procedure Log
     (Empire  : Root_Empire_Type'Class;
      Message : String)
   is
      use type Concorde.Dates.Date_Type;
      Today : constant Concorde.Dates.Date_Type :=
                Concorde.Dates.Current_Date;
   begin
      if not Started then
         return;
      end if;

      if Current_Log_Date = 0 then
         null;
      elsif Current_Log_Date /= Today then
         Flush_Log;
         Current_Log_Date := Today;
      end if;

      Current_Log_Date := Today;
      Current_Logs (Empire.Index).Append (Message);

   end Log;

   -------------------
   -- Start_Logging --
   -------------------

   procedure Start_Logging is
   begin
      if not Started then
         Current_Logs.Clear;
         for I in 1 .. Empire_Count loop
            Current_Logs.Append (List_Of_Log_Lines.Empty_List);
            declare
               File : Ada.Text_IO.File_Type;
            begin
               Ada.Text_IO.Create (File, Ada.Text_IO.Out_File,
                                   Log_File_Path (Vector (I)));
               Ada.Text_IO.Close (File);
            end;
         end loop;

         Current_Log_Date := 0;
         Started := True;
      end if;
   end Start_Logging;

   ------------------
   -- Stop_Logging --
   ------------------

   procedure Stop_Logging is
   begin
      if Started then
         Flush_Log;
         Started := False;
      end if;
   end Stop_Logging;

end Concorde.Empires.Logging;
