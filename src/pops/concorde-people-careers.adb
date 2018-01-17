with Ada.Text_IO;

package body Concorde.People.Careers is

   ----------------------
   -- Promotion_Chance --
   ----------------------

   function Promotion_Chance
     (Career    : Root_Career_Type'Class;
      Rank      : Rank_Index;
      Candidate : Career_Interface'Class)
      return Unit_Real
   is
      Score  : Natural := 0;
      Target : Natural := 0;
      Chance : constant array (0 .. 6) of Unit_Real :=
                 (0.1, 0.3, 0.5, 0.6, 0.7, 0.8, 0.9);
   begin
      if Rank = Career.Number_Of_Ranks then
         return 0.0;
      else
         for Item of Career.Advancement loop
            declare
               Candidate_Rank : Natural := 0;
               Required_Rank  : Natural := 0;
            begin
               case Item.Q is
                  when Education =>
                     Candidate_Rank := Candidate.Education;
                     Required_Rank  := Item.Education_Level;
                  when Skill_Check =>
                     Candidate_Rank :=
                       Natural (Candidate.Level (Item.Skill));
                     Required_Rank :=
                       Natural (Item.Skill_Check_Level);
                  when Ability_Check =>
                     Candidate_Rank :=
                       Natural (Candidate.Ability_Score (Item.Ability));
                     Required_Rank :=
                       Natural (Item.Ability_Check_Level);
               end case;

               Score := Score + Candidate_Rank;

               if not Item.Bonus then
                  Target := Target + Required_Rank;
               end if;
            end;
         end loop;

         return Result : constant Unit_Real :=
           (if Target > Score
            then 0.0
            elsif Score - Target > Chance'Last
            then 0.95
            else Chance (Score - Target))
         do
            if Result > 0.0 then
               Ada.Text_IO.Put_Line
                 ("promotion chance:"
                  & Natural'Image (Natural (Result * 100.0)) & "%");
            end if;
         end return;
      end if;
   end Promotion_Chance;

   ---------------
   -- Qualified --
   ---------------

   function Qualified
     (Career    : Root_Career_Type'Class;
      Candidate : Career_Interface'Class)
      return Boolean
   is
      use Concorde.People.Abilities, Concorde.People.Skills;
   begin
      for Item of Career.Qualifications loop
         case Item.Q is
            when Education =>
               if Candidate.Education < Item.Education_Level then
                  return False;
               end if;
            when Skill_Check =>
               if Candidate.Level (Item.Skill) < Item.Skill_Check_Level then
                  return False;
               end if;
            when Ability_Check =>
               if Candidate.Ability_Score (Item.Ability)
                 < Item.Ability_Check_Level
               then
                  return False;
               end if;
         end case;
      end loop;
      return True;
   end Qualified;

   ------------------
   -- Scan_Careers --
   ------------------

   procedure Scan_Careers
     (Process : not null access
        procedure (Career : Career_Type))
   is
   begin
      Db.Scan (Process);
   end Scan_Careers;

end Concorde.People.Careers;
