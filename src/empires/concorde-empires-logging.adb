with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Text_IO;

with Memor.Element_Vectors;

with Concorde.Dates;
with Concorde.Paths;

with Concorde.Empires.Db;

package body Concorde.Empires.Logging is

   Started : Boolean := False;

   package List_Of_Log_Lines is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   package Empire_Log_Vectors is
     new Memor.Element_Vectors
       (Element_Type  => List_Of_Log_Lines.List,
        Default_Value => List_Of_Log_Lines.Empty_List,
        "="           => List_Of_Log_Lines."=");

   Current_Log_Date : Concorde.Dates.Date_Type := Concorde.Dates.Zero_Date;
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

      procedure Flush (Empire : Empire_Type);

      -----------
      -- Flush --
      -----------

      procedure Flush (Empire : Empire_Type) is
         Lines : constant List_Of_Log_Lines.List :=
                   Current_Logs.Element (Empire.Reference);
      begin
         if not Lines.Is_Empty then
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

               for Line of Lines loop
                  Put_Line (File, Line);
               end loop;
               Close (File);
               Current_Logs.Replace_Element (Empire.Reference,
                                             List_Of_Log_Lines.Empty_List);
            end;
         end if;
      end Flush;

   begin
      if not Started then
         return;
      end if;

      Db.Scan (Flush'Access);

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

      if Current_Log_Date = Concorde.Dates.Zero_Date then
         null;
      elsif Current_Log_Date /= Today then
         Flush_Log;
         Current_Log_Date := Today;
      end if;

      Current_Log_Date := Today;

      declare
         procedure Append (List : in out List_Of_Log_Lines.List);

         ------------
         -- Append --
         ------------

         procedure Append (List : in out List_Of_Log_Lines.List) is
         begin
            List.Append (Message);
         end Append;

      begin

         Current_Logs.Update_Element (Empire.Reference, Append'Access);
      end;

   end Log;

   -------------------
   -- Start_Logging --
   -------------------

   procedure Start_Logging is

      Failed : Boolean := False;

      procedure Start_Empire_Logging (Empire : Empire_Type);

      --------------------------
      -- Start_Empire_Logging --
      --------------------------

      procedure Start_Empire_Logging (Empire : Empire_Type) is
         File : Ada.Text_IO.File_Type;
      begin
         Ada.Text_IO.Create (File, Ada.Text_IO.Out_File,
                             Log_File_Path (Empire));
         Ada.Text_IO.Close (File);
      exception
         when Ada.Text_IO.Name_Error =>
            Failed := True;
      end Start_Empire_Logging;

   begin
      if not Started then
         Current_Logs.Clear;

         Db.Scan (Start_Empire_Logging'Access);

         if Failed then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Unabled to start logging; logging disabled");
         else
            Current_Log_Date := Concorde.Dates.Zero_Date;
            Started := True;
         end if;

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
