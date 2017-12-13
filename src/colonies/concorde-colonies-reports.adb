with Ada.Text_IO;

with WL.Quantities;

with Concorde.People.Pops;
with Concorde.Worlds;

package body Concorde.Colonies.Reports is

   procedure Report_World_Colony
     (World : Concorde.Worlds.World_Type);

   ---------------------
   -- Report_Colonies --
   ---------------------

   procedure Report_Colonies is
   begin
      Concorde.Worlds.Scan_Market_Worlds (Report_World_Colony'Access);
   end Report_Colonies;

   procedure Report_World_Colony
     (World : Concorde.Worlds.World_Type)
   is
      use Ada.Text_IO;

      Total_Population : WL.Quantities.Quantity_Type :=
                           WL.Quantities.Zero;

      procedure Add_Population
        (Pop : Concorde.People.Pops.Pop_Type);

      --------------------
      -- Add_Population --
      --------------------

      procedure Add_Population
        (Pop : Concorde.People.Pops.Pop_Type)
      is
         use type WL.Quantities.Quantity_Type;
      begin
         Total_Population := Total_Population + Pop.Size_Quantity;
      end Add_Population;

   begin
      World.Scan_Pops (Add_Population'Access);
      Put (World.Name);
      Set_Col (32);
      Put (WL.Quantities.Show (Total_Population));
      New_Line;
   end Report_World_Colony;

end Concorde.Colonies.Reports;
