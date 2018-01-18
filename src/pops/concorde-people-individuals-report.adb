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
      use Concorde.Calendar;
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

      if not Individual.Career.Is_Empty then
         New_Line;
         Put_Line ("Career             Start Date  End Date    Rank");
         for Item of Individual.Career loop
            Put (Item.Career.Name);
            Set_Col (20);
            Put (Concorde.Calendar.Image (Item.Start));
            if Item.Finish < Concorde.Calendar.Clock then
               Set_Col (32);
               Put (Concorde.Calendar.Image (Item.Finish));
            end if;
            Set_Col (44);
            Put (Item.Career.Rank_Name (Item.Rank));
            New_Line;
         end loop;
         New_Line;
      end if;

      declare

         First_Skill : Boolean := True;

         procedure Show_Skill
           (Skill : Concorde.People.Skills.Skill_Type;
            Level : Concorde.People.Skills.Skill_Level_Range);

         ----------------
         -- Show_Skill --
         ----------------

         procedure Show_Skill
           (Skill : Concorde.People.Skills.Skill_Type;
            Level : Concorde.People.Skills.Skill_Level_Range)
         is
         begin
            if First_Skill then
               Put ("Skills: ");
            else
               Put (", ");
            end if;
            First_Skill := False;

            Put (Skill.Name
                 & Concorde.People.Skills.Skill_Level_Range'Image (Level));
         end Show_Skill;

      begin
         Individual.Scan (Show_Skill'Access);
         if not First_Skill then
            New_Line;
         end if;
      end;

      New_Line;
      Set_Output (Standard_Output);
      Close (File);
   end Report;

end Concorde.People.Individuals.Report;
