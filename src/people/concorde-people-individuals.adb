with Ada.Characters.Handling;

with Concorde.Locations;
with Concorde.Surfaces;
with Concorde.Worlds;

with Concorde.People.Communities;

package body Concorde.People.Individuals is

   -------------------
   -- Ability_Score --
   -------------------

   overriding function Ability_Score
     (Individual : Root_Individual_Type;
      Ability    : Concorde.People.Abilities.Ability_Type)
      return Concorde.People.Abilities.Ability_Score_Range
   is
      use Concorde.Calendar;
      use Concorde.People.Abilities;
   begin
      if Individual.Age < 20 then
         declare
            Age_Duration : constant Real :=
                             Real (Clock - Individual.Birth);
            Maturity_Duration : constant Real :=
                                  Real (Years (20));
            Final_Score       : constant Ability_Score_Range :=
                                  Individual.Abilities (Ability);
         begin
            return Ability_Score_Range
              (Real (Final_Score) * Age_Duration / Maturity_Duration);
         end;
      else
         return Individual.Abilities (Ability);
      end if;
   end Ability_Score;

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

   ------------------
   -- Clear_Office --
   ------------------

   procedure Clear_Office
     (Individual : in out Root_Individual_Type'Class)
   is
   begin
      Individual.Current_Office := null;
   end Clear_Office;

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

   -----------------
   -- Set_Manager --
   -----------------

   overriding procedure Set_Manager
     (Individual  : in out Root_Individual_Type;
      Manager     : Concorde.Managers.Manager_Type)
   is
   begin
      Individual.Manager := Manager;
   end Set_Manager;

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

   ----------------
   -- Set_Office --
   ----------------

   procedure Set_Office
     (Individual : in out Root_Individual_Type'Class;
      Office     : not null access constant
        Concorde.Ministries.Root_Ministry_Type'Class)
   is
   begin
      Individual.Current_Office := Ministry_Access (Office);
   end Set_Office;

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

   ---------------
   -- Set_Trait --
   ---------------

   overriding procedure Set_Trait
     (Individual : in out Root_Individual_Type;
      Trait      : Concorde.People.Traits.Trait_Type;
      Present    : Boolean)
   is
   begin
      if Present then
         if not Individual.Has_Trait (Trait) then
            Individual.Traits.Append (Trait);
         end if;
      else
         declare
            use Concorde.People.Traits.Lists;
            Position : Cursor := Individual.Traits.Find (Trait);
         begin
            if Has_Element (Position) then
               Individual.Traits.Delete (Position);
            end if;
         end;
      end if;
   end Set_Trait;

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

   ------------------
   -- Update_Agent --
   ------------------

   overriding procedure Update_Agent
     (Individual     : not null access constant Root_Individual_Type;
      Perform_Update : not null access
        procedure (Agent : in out Concorde.Agents.Root_Agent_Type'Class))
   is
   begin
      Perform_Update (Individual.Update);
   end Update_Agent;

   ---------------------
   -- Update_Location --
   ---------------------

   procedure Update_Location
     (Individual : Individual_Type)
   is
   begin
      if Individual.Is_Community_Location then
         Individual.Current_Community.Update.Add_Individual
           (Individual);
      else
         raise Constraint_Error with
           "invalid location for " & Individual.Full_Name
           & ": " & Concorde.Locations.Long_Name
           (Individual.Current_Location);
      end if;
   end Update_Location;

end Concorde.People.Individuals;
