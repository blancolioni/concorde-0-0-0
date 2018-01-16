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
   begin
      if Rank = Career.Number_Of_Ranks then
         return 0.0;
      else
         return Chance : Unit_Real := 1.0 do
            for Item of Career.Qualifications loop
               declare
                  Power : Natural := 0;
               begin
                  case Item.Q is
                     when Education =>
                        Power := Candidate.Education - Item.Education_Level;
                     when Skill_Check =>
                        Power :=
                          Natural (Candidate.Level (Item.Skill))
                            - Natural (Item.Skill_Check_Level);
                     when Ability_Check =>
                        Power :=
                          Natural (Candidate.Ability_Score (Item.Ability))
                            - Natural (Item.Ability_Check_Level);
                  end case;

                  if Power < Natural (Rank) then
                     Chance := Chance * 0.1;
                  elsif Power = Natural (Rank) then
                     Chance := Chance * 0.5;
                  elsif Power - Natural (Rank) < 5 then
                     Chance := Chance *
                       (0.75 + Real (Power - Natural (Rank)) / 20.0);
                  else
                     Chance := Chance * 1.0;
                  end if;
               end;
            end loop;
            Ada.Text_IO.Put_Line
              ("promotion chance:"
               & Natural'Image (Natural (Chance * 100.0)) & "%");
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
