with Ada.Text_IO;

with WL.Localisation;

with Accord.Objects;

package body Accord.Politics.Report is

   ------------
   -- Report --
   ------------

   procedure Report
     (Politic : Political_Interface'Class)
   is
      use Ada.Text_IO;
   begin
      if Politic in Accord.Objects.Named_Object_Interface'Class then
         declare
            Name : constant String :=
                     Accord.Objects.Named_Object_Interface'Class
                       (Politic).Name;
         begin
            Put_Line (Name);
         end;
      end if;

      for Axis in Political_Axis loop
         Put (WL.Localisation.Local_Text (Axis'Image));
         Set_Col (23);
         Put (Natural'Image (Natural (Politic.Position (Axis) * 100.0)));
         Set_Col (29);
         Put (Natural'Image (Natural (Politic.Strength (Axis) * 100.0)));
         New_Line;
      end loop;
   end Report;

end Accord.Politics.Report;
