with Ada.Text_IO;

package body Concorde.Empires.Reports is

   --------------------
   -- Report_Empires --
   --------------------

   procedure Report_Empires is
      use Ada.Text_IO;
   begin
      Put ("Name");
      Set_Col (24);
      Put ("Systems");
      Set_Col (32);
      Put ("Ships");
      Set_Col (40);
      Put ("Built");
      Set_Col (48);
      Put ("Lost");
      New_Line;

      for Empire of Vector loop
         Put (Empire.Name);
         Set_Col (24);
         Put (Lui.Approximate_Image (Empire.Current_Systems));
         Set_Col (32);
         Put (Lui.Approximate_Image (Empire.Current_Ships));
         Set_Col (40);
         Put (Lui.Approximate_Image (Empire.Built_Ships));
         Set_Col (48);
         Put (Lui.Approximate_Image (Empire.Lost_Ships));
         New_Line;
      end loop;
   end Report_Empires;

end Concorde.Empires.Reports;
