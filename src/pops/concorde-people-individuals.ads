private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

private with Memor;
private with Memor.Database;

with Concorde.Agents;
with Concorde.Factions;
with Concorde.Objects;
with Concorde.Trades;

with Concorde.Calendar;

with Concorde.People.Careers;
with Concorde.People.Genetics;
with Concorde.People.Groups;

with Concorde.People.Abilities;
with Concorde.People.Proficiencies;
with Concorde.People.Skills;
with Concorde.People.Attributes;

package Concorde.People.Individuals is

   type Gender_Type is (Female, Male, None);

   type Root_Individual_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Factions.Citizen_Interface
     and Concorde.Objects.User_Named_Object_Interface
     and Concorde.People.Skills.Has_Skills_Interface
     and Concorde.People.Attributes.Has_Attributes
     and Concorde.People.Careers.Career_Interface
   with private;

   overriding function Ability_Score
     (Individual : Root_Individual_Type;
      Ability    : Concorde.People.Abilities.Ability_Type)
      return Concorde.People.Abilities.Ability_Score_Range;

   overriding function Skill_Level
     (Individual : Root_Individual_Type;
      Skill      : Concorde.People.Skills.Skill_Type)
      return Concorde.People.Skills.Skill_Level_Range;

   overriding procedure Scan
     (Individual : Root_Individual_Type;
      Process    : not null access
        procedure (Skill : Concorde.People.Skills.Skill_Type;
                   Level : Concorde.People.Skills.Skill_Level_Range));

   overriding function Proficiency_Level
     (Individual  : Root_Individual_Type;
      Proficiency : Concorde.People.Proficiencies.Proficiency_Type)
      return Concorde.People.Proficiencies.Proficiency_Score_Range;

   overriding procedure Set_Ability_Score
     (Individual : in out Root_Individual_Type;
      Ability    : Concorde.People.Abilities.Ability_Type;
      Score      : Concorde.People.Abilities.Ability_Score_Range);

   overriding procedure Set_Proficiency_Level
     (Individual  : in out Root_Individual_Type;
      Proficiency : Concorde.People.Proficiencies.Proficiency_Type;
      Level       : Concorde.People.Proficiencies.Proficiency_Score_Range);

   overriding procedure Set_Skill_Level
     (Individual : in out Root_Individual_Type;
      Skill      : Concorde.People.Skills.Skill_Type;
      Level      : Concorde.People.Skills.Skill_Level_Range);

   function Last_Name (Individual : Root_Individual_Type'Class)
                       return String;

   function First_Name (Individual : Root_Individual_Type'Class)
                        return String;

   function Full_Name (Individual : Root_Individual_Type'Class)
                       return String;

   function Age
     (Individual : Root_Individual_Type'Class)
      return Natural;

   function Qualified
     (Individual : Root_Individual_Type'Class;
      Career     : Concorde.People.Careers.Career_Type)
      return Boolean;

   function Has_Career
     (Individual : Root_Individual_Type'Class;
      Career     : Concorde.People.Careers.Career_Type)
      return Boolean;

   function Career_Rank
     (Individual : Root_Individual_Type'Class;
      Career     : Concorde.People.Careers.Career_Type)
      return Concorde.People.Careers.Rank_Index
     with Pre => Individual.Has_Career (Career);

   type Individual_Type is access constant Root_Individual_Type'Class;

   type Updateable_Reference
     (Item : not null access Root_Individual_Type'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Root_Individual_Type'Class)
      return Updateable_Reference;

private

   type Career_Record is
      record
         Career : Concorde.People.Careers.Career_Type;
         Start  : Concorde.Calendar.Time;
         Finish : Concorde.Calendar.Time;
         Rank   : Concorde.People.Careers.Rank_Index;
      end record;

   package List_Of_Career_Records is
     new Ada.Containers.Doubly_Linked_Lists (Career_Record);

   type Ability_Score_Array is
     array (Concorde.People.Abilities.Ability_Type)
     of Concorde.People.Abilities.Ability_Score_Range;

   type Proficiency_Level_Array is
     array (Concorde.People.Proficiencies.Proficiency_Type)
     of Concorde.People.Proficiencies.Proficiency_Score_Range;

   type Root_Individual_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Factions.Citizen_Interface
     and Concorde.Objects.User_Named_Object_Interface
     and Concorde.People.Skills.Has_Skills_Interface
     and Concorde.People.Careers.Career_Interface with
      record
         Title               : Ada.Strings.Unbounded.Unbounded_String;
         First_Name          : Ada.Strings.Unbounded.Unbounded_String;
         Last_Name           : Ada.Strings.Unbounded.Unbounded_String;
         DNA                 : Concorde.People.Genetics.Genome;
         Gender              : Gender_Type;
         Partner_Gender      : Gender_Type;
         Preference_Strength : Unit_Real;
         Birth               : Concorde.Calendar.Time;
         Death               : Concorde.Calendar.Time;
         Alive               : Boolean;
         Faction             : Concorde.Factions.Faction_Type;
         Citizenship         : Concorde.Factions.Faction_Type;
         Group               : Concorde.People.Groups.Pop_Group;
         Loyalty             : Unit_Real;
         Abilities           : Ability_Score_Array;
         Proficiencies       : Proficiency_Level_Array :=
                                 (others => 0);
         Career              : List_Of_Career_Records.List;
         Skills              : Concorde.People.Skills.Skill_Set;
      end record;

   overriding function Object_Database
     (Item : Root_Individual_Type)
      return Memor.Memor_Database;

   overriding function Class_Name
     (Individual : Root_Individual_Type) return String
   is ("individual");

   overriding function Name
     (Individual : Root_Individual_Type)
      return String
   is (Ada.Strings.Unbounded.To_String (Individual.First_Name)
       & " "
       & Ada.Strings.Unbounded.To_String (Individual.Last_Name));

   overriding procedure Set_Name
     (Individual : in out Root_Individual_Type;
      New_Name   : String);

   overriding function Short_Name
     (Item : Root_Individual_Type)
      return String
   is (Item.Name);

   overriding function Citizenship
     (Item : Root_Individual_Type)
      return Concorde.Factions.Faction_Type
   is (Item.Citizenship);

   overriding function Variable_Reference
     (Individual : not null access constant Root_Individual_Type)
      return access Concorde.Agents.Root_Agent_Type'Class
   is (Individual.Update.Item);

   overriding function Skill_Level
     (Individual : Root_Individual_Type;
      Skill      : Concorde.People.Skills.Skill_Type)
      return Concorde.People.Skills.Skill_Level_Range
   is (Individual.Skills.Skill_Level (Skill));

   overriding function Ability_Score
     (Individual : Root_Individual_Type;
      Ability    : Concorde.People.Abilities.Ability_Type)
      return Concorde.People.Abilities.Ability_Score_Range
   is (Individual.Abilities (Ability));

   overriding function Proficiency_Level
     (Individual  : Root_Individual_Type;
      Proficiency : Concorde.People.Proficiencies.Proficiency_Type)
      return Concorde.People.Proficiencies.Proficiency_Score_Range
   is (Individual.Proficiencies (Proficiency));

   function Last_Name (Individual : Root_Individual_Type'Class)
                       return String
   is (Ada.Strings.Unbounded.To_String (Individual.Last_Name));

   function First_Name (Individual : Root_Individual_Type'Class)
                        return String
   is (Ada.Strings.Unbounded.To_String (Individual.First_Name));

   package Db is
     new Memor.Database
       ("individual", Root_Individual_Type, Individual_Type);

   overriding function Object_Database
     (Item : Root_Individual_Type)
      return Memor.Memor_Database
   is (Db.Get_Database);

   type Updateable_Reference
     (Item : not null access Root_Individual_Type'Class)
   is
      record
         Update : Db.Updateable_Reference (Item);
      end record;

end Concorde.People.Individuals;
