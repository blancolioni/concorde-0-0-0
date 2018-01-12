with Ada.Text_IO;

package body Concorde.People.Individuals.Report is

   First : Boolean := True;

   ------------
   -- Report --
   ------------

   procedure Report
     (Individual : not null access constant Root_Individual_Type'Class)
   is
      use Ada.Text_IO;
      File : File_Type;
   begin
      if First then
         Create (File, Out_File, "individuals.txt");
         Close (File);
         First := False;
      end if;

      Open (File, Append_File, "individuals.txt");
      Set_Output (File);
      Put_Line (Individual.Full_Name & " born "
                & Concorde.Calendar.Image (Individual.Birth)
                & " (age" & Natural'Image (Individual.Age) & ")");
      for Ability in Individual.Abilities'Range loop
         Put (Ability_Type'Image (Ability) & ":");
         Set_Col (16);
         Put (Individual.Abilities (Ability)'Img);
         New_Line;
      end loop;
      New_Line;
      Set_Output (Standard_Output);
      Close (File);
   end Report;

end Concorde.People.Individuals.Report;
