with Ada.Text_IO;

with Concorde.Agents.Reports;

with Concorde.Paths;

package body Concorde.Reports is

   procedure Write_Agent_Accounts (Agent : Concorde.Agents.Agent_Type);

   --------------------
   -- Write_Accounts --
   --------------------

   procedure Write_Accounts is
   begin
      Concorde.Agents.Scan_Agents (Write_Agent_Accounts'Access);
   end Write_Accounts;

   --------------------------
   -- Write_Agent_Accounts --
   --------------------------

   procedure Write_Agent_Accounts (Agent : Concorde.Agents.Agent_Type) is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Create (File, Out_File,
              Concorde.Paths.Config_Path
              & "/../log/accounts/"
              & Agent.Class_Name
              & "-"
              & Agent.Identifier
              & ".txt");
      Set_Output (File);
      Concorde.Agents.Reports.Write_Accounts (Agent);
      Set_Output (Standard_Output);
      Close (File);
   end Write_Agent_Accounts;

end Concorde.Reports;
