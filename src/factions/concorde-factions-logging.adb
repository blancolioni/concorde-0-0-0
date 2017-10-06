with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Text_IO;

with Memor.Element_Vectors;

with Concorde.Calendar;
with Concorde.Paths;

package body Concorde.Factions.Logging is

   Started : Boolean := False;

   package List_Of_Log_Lines is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   package Faction_Log_Vectors is
     new Memor.Element_Vectors
       (Index_Type    => Root_Faction_Type,
        Element_Type  => List_Of_Log_Lines.List,
        Default_Value => List_Of_Log_Lines.Empty_List,
        "="           => List_Of_Log_Lines."=");

   Current_Log_Date : Concorde.Calendar.Time;
   Current_Logs     : Faction_Log_Vectors.Vector;

   function Log_File_Path
     (Faction : Faction_Type)
      return String
   is (Concorde.Paths.Config_Path & "/../log/factions/"
       & Faction.Name
       & ".txt");

   ---------------
   -- Flush_Log --
   ---------------

   procedure Flush_Log is

      procedure Flush (Faction : Faction_Type);

      -----------
      -- Flush --
      -----------

      procedure Flush (Faction : Faction_Type) is
         Lines : constant List_Of_Log_Lines.List :=
                   Current_Logs.Element (Faction);
      begin
         if not Lines.Is_Empty then
            declare
               use Ada.Text_IO;
               File : File_Type;
            begin
               Open (File, Append_File, Log_File_Path (Faction));
               New_Line (File);
               Put_Line (File, "----------------");
               Put_Line (File,
                         "-- "
                         & Concorde.Calendar.Image
                           (Current_Log_Date,
                            Include_Time_Fraction => True)
                         & " --");
               Put_Line (File, "----------------");
               New_Line (File);

               for Line of Lines loop
                  Put_Line (File, Line);
               end loop;
               Close (File);
               Current_Logs.Replace_Element (Faction,
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
     (Faction  : not null access constant Root_Faction_Type'Class;
      Message : String)
   is
   begin
      Log (Faction.all, Message);
   end Log;

   ---------
   -- Log --
   ---------

   procedure Log
     (Faction  : Root_Faction_Type'Class;
      Message : String)
   is
      use Concorde.Calendar;
      Now   : constant Time := Clock;
      Today : constant Time := Now - Seconds (Now);
   begin
      if not Started then
         return;
      end if;

      if Current_Log_Date /= Today then
         Flush_Log;
         Current_Log_Date := Today;
      end if;

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

         Current_Logs.Update_Element (Faction, Append'Access);
      end;

   end Log;

   -------------------
   -- Start_Logging --
   -------------------

   procedure Start_Logging is

      Failed : Boolean := False;

      procedure Start_Faction_Logging (Faction : Faction_Type);

      --------------------------
      -- Start_Faction_Logging --
      --------------------------

      procedure Start_Faction_Logging (Faction : Faction_Type) is
         File : Ada.Text_IO.File_Type;
      begin
         Ada.Text_IO.Create (File, Ada.Text_IO.Out_File,
                             Log_File_Path (Faction));
         Ada.Text_IO.Close (File);
      exception
         when Ada.Text_IO.Name_Error =>
            Failed := True;
      end Start_Faction_Logging;

   begin
      if not Started then
         Current_Logs.Clear;

         Db.Scan (Start_Faction_Logging'Access);

         if Failed then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Unable to start faction logging; logging disabled");
         else
            declare
               use Concorde.Calendar;
               Now : constant Time := Clock;
            begin
               Current_Log_Date := Now - Seconds (Now);
               Started := True;
            end;
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

end Concorde.Factions.Logging;
