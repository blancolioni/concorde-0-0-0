with Ada.Text_IO;

package body Concorde.Factions.Reports is

   --------------------
   -- Report_Factions --
   --------------------

   procedure Report_Factions is
      use Ada.Text_IO;

      procedure Report (Faction : Faction_Type);

      ------------
      -- Report --
      ------------

      procedure Report (Faction : Faction_Type) is
      begin
         Put (Faction.Name);
         Set_Col (24);
         Put (Lui.Approximate_Image (Faction.Current_Systems));
         Set_Col (32);
         Put (Lui.Approximate_Image (Faction.Current_Ships));
         Set_Col (40);
         Put (Lui.Approximate_Image (Faction.Built_Ships));
         Set_Col (48);
         Put (Lui.Approximate_Image (Faction.Captured_Ships));
         Set_Col (56);
         Put (Lui.Approximate_Image (Faction.Lost_Ships));
         Set_Col (64);
         Put (Lui.Approximate_Image (Faction.Destroyed_Ships));
         New_Line;
      end Report;

   begin
      Put ("Name");
      Set_Col (24);
      Put ("Systems");
      Set_Col (32);
      Put ("Ships");
      Set_Col (40);
      Put ("Built");
      Set_Col (48);
      Put ("Captured");
      Set_Col (56);
      Put ("Lost");
      Set_Col (64);
      Put ("Destroyed");
      New_Line;

      Db.Scan (Report'Access);

   end Report_Factions;

end Concorde.Factions.Reports;
