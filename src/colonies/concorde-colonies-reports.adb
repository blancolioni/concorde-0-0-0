with Concorde.People.Communities;

package body Concorde.Colonies.Reports is

   procedure Report_Community
     (Community : Concorde.People.Communities.Community_Type);

   ---------------------
   -- Report_Colonies --
   ---------------------

   procedure Report_Colonies is
   begin
      Concorde.People.Communities.Scan
        (Report_Community'Access);
   end Report_Colonies;

   procedure Report_Community
     (Community : Concorde.People.Communities.Community_Type)
   is null;
--        use Ada.Text_IO;
--
--        Total_Population : Concorde.Quantities.Quantity_Type :=
--                             Concorde.Quantities.Zero;
--
--        procedure Add_Population
--          (Pop : Concorde.People.Pops.Pop_Type);
--
--        --------------------
--        -- Add_Population --
--        --------------------
--
--        procedure Add_Population
--          (Pop : Concorde.People.Pops.Pop_Type)
--        is
--           use type Concorde.Quantities.Quantity_Type;
--        begin
--           Total_Population := Total_Population + Pop.Size_Quantity;
--        end Add_Population;
--
--     begin
--        World.Scan_Pops (Add_Population'Access);
--        Put (World.Name);
--        Set_Col (32);
--        Put (Concorde.Quantities.Show (Total_Population));
--        New_Line;
--     end Report_World_Colony;

end Concorde.Colonies.Reports;
