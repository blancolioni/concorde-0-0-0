with Tropos.Reader;

with Concorde.Paths;

package body Concorde.Scenarios is

   type Scenario_Record is
      record
         Imperial_Centre : Boolean := False;
      end record;

   Current_Scenario : Scenario_Record;

   ---------------------
   -- Imperial_Centre --
   ---------------------

   function Imperial_Centre return Boolean is
   begin
      return Current_Scenario.Imperial_Centre;
   end Imperial_Centre;

   -------------------
   -- Load_Scenario --
   -------------------

   procedure Load_Scenario (Name : String) is
      Config : constant Tropos.Configuration :=
                 Tropos.Reader.Read_Config
                   (Concorde.Paths.Config_File
                      ("scenarios/" & Name & "/" & Name & ".scenario"));
   begin
      Current_Scenario :=
        (Imperial_Centre => Config.Get ("imperial_centre"));
   end Load_Scenario;

end Concorde.Scenarios;
