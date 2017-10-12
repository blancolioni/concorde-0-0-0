with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Directories;
with Ada.Strings.Fixed;

with Ada.Text_IO;

with WL.Processes;
with WL.String_Maps;

with Concorde.Calendar;
with Concorde.Options;

package body Concorde.Logs is

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   package Log_File_Maps is
     new WL.String_Maps (String_Lists.List, String_Lists."=");

   Log_Files : Log_File_Maps.Map;

   procedure Ensure_Path
     (Base_Path  : String;
      Local_Path : String);

   -----------------
   -- Ensure_Path --
   -----------------

   procedure Ensure_Path
     (Base_Path  : String;
      Local_Path : String)
   is
      use Ada.Strings.Fixed;
      Slash : constant Natural := Index (Local_Path, "/");
   begin
      if Slash > 0 then
         declare
            Folder_Name : constant String :=
                            Local_Path (Local_Path'First .. Slash - 1);
            Folder_Path : constant String :=
                            Ada.Directories.Compose
                              (Base_Path, Folder_Name);
         begin
            if not Ada.Directories.Exists (Folder_Path) then
               Ada.Directories.Create_Directory (Folder_Path);
            end if;
            Ensure_Path (Folder_Path,
                         Local_Path (Slash + 1 .. Local_Path'Last));
         end;
      end if;
   end Ensure_Path;

   ----------------
   -- Flush_Logs --
   ----------------

   procedure Flush_Logs is
      Base_Path : constant String := Concorde.Options.Log_Folder;
      Process : WL.Processes.Process_Type;
   begin
      if Log_Files.Is_Empty then
         return;
      end if;

      Process.Start_Percentage
        ("Writing log files", Positive (Log_Files.Length));

      for Position in Log_Files.Iterate loop
         declare
            use Ada.Text_IO;
            Local_Path : constant String :=
                           Log_File_Maps.Key (Position);
            Full_Path  : constant String :=
                           Base_Path & "/" & Local_Path & ".txt";
            File       : File_Type;
         begin
            Ensure_Path (Base_Path, Local_Path);
            Create (File, Out_File, Full_Path);
            for Line of Log_File_Maps.Element (Position) loop
               Put_Line (File, Line);
            end loop;
            Close (File);
            Process.Tick;
         end;
      end loop;
      Process.Finish;
      Log_Files.Clear;

   end Flush_Logs;

   --------------
   -- Log_Line --
   --------------

   procedure Log_Line
     (Log_Path : String;
      Line     : String)
   is
   begin
      if not Log_Files.Contains (Log_Path) then
         Log_Files.Insert (Log_Path, String_Lists.Empty_List);
      end if;

      Log_Files (Log_Path).Append
        (Concorde.Calendar.Image (Concorde.Calendar.Clock)
         & "," & Line);
   end Log_Line;

end Concorde.Logs;
