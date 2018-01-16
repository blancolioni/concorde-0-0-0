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
         Put (Concorde.People.Abilities.Ability_Type'Image (Ability) & ":");
         Set_Col (16);
         Put (Individual.Abilities (Ability)'Img);
         New_Line;
      end loop;
      New_Line;

      declare

         First_Skill : Boolean := True;

         procedure Show_Skill
           (Skill : Concorde.People.Skills.Skill_Type;
            Level : Concorde.People.Skills.Skill_Level);

         ----------------
         -- Show_Skill --
         ----------------

         procedure Show_Skill
           (Skill : Concorde.People.Skills.Skill_Type;
            Level : Concorde.People.Skills.Skill_Level)
         is
         begin
            if First_Skill then
               Put ("Skills: ");
            else
               Put (", ");
            end if;
            First_Skill := False;

            Put (Skill.Name
                 & Concorde.People.Skills.Skill_Level'Image (Level));
         end Show_Skill;

      begin
         Individual.Scan (Show_Skill'Access);
         if not First_Skill then
            New_Line;
         end if;
      end;

      Set_Output (Standard_Output);
      Close (File);
   end Report;

end Concorde.People.Individuals.Report;
