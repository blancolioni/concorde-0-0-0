with Ada.Characters.Handling;

package body Concorde.People.Individuals is

   ---------
   -- Age --
   ---------

   function Age
     (Individual : Root_Individual_Type'Class)
      return Natural
   is
      use Concorde.Calendar;
      Seconds : constant Duration := Clock - Individual.Birth;
      Days    : constant Non_Negative_Real := Real (Seconds) / 86_600.0;
      Years   : constant Non_Negative_Real := Days / 360.0;
   begin
      return Natural (Real'Truncation (Years));
   end Age;

   -----------------
   -- Career_Rank --
   -----------------

   function Career_Rank
     (Individual : Root_Individual_Type'Class;
      Career     : Concorde.People.Careers.Career_Type)
      return Concorde.People.Careers.Rank_Index
   is
      use type Concorde.People.Careers.Career_Type;
   begin
      for Rec of Individual.Career loop
         if Rec.Career = Career then
            return Rec.Rank;
         end if;
      end loop;
      raise Program_Error;
   end Career_Rank;

   ---------------
   -- Full_Name --
   ---------------

   function Full_Name (Individual : Root_Individual_Type'Class)
                       return String
   is
      Title      : constant String :=
                     Ada.Strings.Unbounded.To_String (Individual.Title);
      First_Name : constant String :=
                     Ada.Strings.Unbounded.To_String (Individual.First_Name);
      Last_Name  : constant String :=
                     Ada.Strings.Unbounded.To_String (Individual.Last_Name);
   begin
      if Title /= "" then
         return Title & " " & First_Name & " " & Last_Name;
      else
         return First_Name & " " & Last_Name;
      end if;
   end Full_Name;

   ----------------
   -- Has_Career --
   ----------------

   function Has_Career
     (Individual : Root_Individual_Type'Class;
      Career     : Concorde.People.Careers.Career_Type)
      return Boolean
   is
      use type Concorde.People.Careers.Career_Type;
   begin
      return (for some Rec of Individual.Career =>
                Rec.Career = Career);
   end Has_Career;

   ---------------
   -- Qualified --
   ---------------

   function Qualified
     (Individual : Root_Individual_Type'Class;
      Career     : Concorde.People.Careers.Career_Type)
      return Boolean
   is
   begin
      return Career.Qualified (Individual);
   end Qualified;

   ----------
   -- Scan --
   ----------

   overriding procedure Scan
     (Individual : Root_Individual_Type;
      Process    : not null access
        procedure (Skill : Concorde.People.Skills.Skill_Type;
                   Level : Concorde.People.Skills.Skill_Level_Range))
   is
   begin
      Individual.Skills.Scan (Process);
   end Scan;

   -----------------------
   -- Set_Ability_Score --
   -----------------------

   overriding procedure Set_Ability_Score
     (Individual : in out Root_Individual_Type;
      Ability    : Concorde.People.Abilities.Ability_Type;
      Score      : Concorde.People.Abilities.Ability_Score_Range)
   is
   begin
      Individual.Abilities (Ability) := Score;
   end Set_Ability_Score;

   --------------
   -- Set_Name --
   --------------

   overriding procedure Set_Name
     (Individual : in out Root_Individual_Type;
      New_Name   : String)
   is
      Name : String := New_Name;
      Cap  : Boolean := True;
   begin
      for Ch of Name loop
         if Cap then
            Ch := Ada.Characters.Handling.To_Upper (Ch);
            Cap := False;
         elsif Ch = '-' then
            Cap := True;
         else
            Ch := Ada.Characters.Handling.To_Lower (Ch);
         end if;
      end loop;
      Individual.First_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Set_Name;

   ---------------------------
   -- Set_Proficiency_Level --
   ---------------------------

   overriding procedure Set_Proficiency_Level
     (Individual  : in out Root_Individual_Type;
      Proficiency : Concorde.People.Proficiencies.Proficiency_Type;
      Level       : Concorde.People.Proficiencies.Proficiency_Score_Range)
   is
   begin
      Individual.Proficiencies (Proficiency) := Level;
   end Set_Proficiency_Level;

   ---------------------
   -- Set_Skill_Level --
   ---------------------

   overriding procedure Set_Skill_Level
     (Individual : in out Root_Individual_Type;
      Skill      : Concorde.People.Skills.Skill_Type;
      Level      : Concorde.People.Skills.Skill_Level_Range)
   is
   begin
      Individual.Skills.Set_Level (Skill, Level);
   end Set_Skill_Level;

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant Root_Individual_Type'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update;

end Concorde.People.Individuals;
