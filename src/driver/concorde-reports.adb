with Ada.Text_IO;

with Memor;

with Concorde.Agents.Reports;

with Concorde.Empires.Db;
with Concorde.Installations.Db;
with Concorde.People.Pops.Db;
with Concorde.Ships.Db;

with Concorde.Paths;

package body Concorde.Reports is

   procedure Write_Agent_Accounts
     (Rec : Memor.Root_Record_Type'Class);

   --------------------
   -- Write_Accounts --
   --------------------

   procedure Write_Accounts is
   begin
      Concorde.Empires.Db.Scan (Write_Agent_Accounts'Access);
      Concorde.Installations.Db.Scan (Write_Agent_Accounts'Access);
      Concorde.People.Pops.Db.Scan (Write_Agent_Accounts'Access);
      Concorde.Ships.Db.Scan (Write_Agent_Accounts'Access);
   end Write_Accounts;

   --------------------------
   -- Write_Agent_Accounts --
   --------------------------

   procedure Write_Agent_Accounts
     (Rec : Memor.Root_Record_Type'Class)
   is
      use Ada.Text_IO;
      File : File_Type;
      Agent : Concorde.Agents.Root_Agent_Type'Class
      renames Concorde.Agents.Root_Agent_Type'Class (Rec);
   begin
      Create (File, Out_File,
              Concorde.Paths.Config_Path
              & "/../log/accounts/"
              & Rec.Object_Database.Database_Class_Name
              & "-"
              & Agent.Identifier
              & ".txt");
      Set_Output (File);
      Concorde.Agents.Reports.Write_Accounts (Agent);
      Set_Output (Standard_Output);
      Close (File);
   end Write_Agent_Accounts;

end Concorde.Reports;
