with Ada.Text_IO;

with Concorde.Worlds;

with Concorde.Markets.Reports;

package body Concorde.People.Communities.Reports is

   ----------------------
   -- Report_Community --
   ----------------------

   procedure Report_Community
     (Community : Community_Type)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("Community on " & Community.World.Name
                & " owned by " & Community.Owner.Name);
      Concorde.Markets.Reports.Report_Market
        (Community.all);
      New_Line;
   end Report_Community;

end Concorde.People.Communities.Reports;
