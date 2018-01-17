private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Memor.Database;
private with Concorde.Localisation;

with Memor;

with Concorde.Objects;

with Concorde.Commodities;
with Concorde.People.Abilities;
with Concorde.People.Skills;

package Concorde.People.Careers is

   type Rank_Count is range 0 .. 6;
   subtype Rank_Index is Rank_Count range 1 .. Rank_Count'Last;

   type Career_Interface is limited interface
     and Concorde.People.Skills.Has_Skills_Interface;

   function Education
     (Item : Career_Interface)
      return Natural
      is abstract;

   function Ability_Score
     (Item : Career_Interface;
      Ability : Concorde.People.Abilities.Ability_Type)
      return Concorde.People.Abilities.Ability_Score_Range
      is abstract;

   type Root_Career_Type is
     new Concorde.Objects.Root_Localised_Object_Type with private;

   function Qualified
     (Career    : Root_Career_Type'Class;
      Candidate : Career_Interface'Class)
      return Boolean;

   function Promotion_Chance
     (Career    : Root_Career_Type'Class;
      Rank      : Rank_Index;
      Candidate : Career_Interface'Class)
      return Unit_Real;

   function Titles
     (Career : Root_Career_Type'Class)
      return Boolean;

   type Career_Type is access constant Root_Career_Type'Class;

   function Exists (Name : String) return Boolean;

   function Get (Name : String) return Career_Type
     with Pre => Exists (Name);

   function Prestige
     (Career : Root_Career_Type'Class)
      return Natural;

   function Number_Of_Ranks
     (Career : Root_Career_Type'Class)
      return Rank_Count;

   function Rank_Name
     (Career : Root_Career_Type'Class;
      Index  : Rank_Index)
      return String
     with Pre => Index <= Career.Number_Of_Ranks;

   type Array_Of_Skills is
     array (Positive range <>) of Concorde.People.Skills.Skill_Type;

   function Rank_Skills
     (Career : Root_Career_Type'Class;
      Index  : Rank_Index)
      return Array_Of_Skills
     with Pre => Index <= Career.Number_Of_Ranks;

   procedure Scan_Careers
     (Process : not null access
        procedure (Career : Career_Type));

private

   type Qualification_Type is (Education, Skill_Check, Ability_Check);

   type Qualification (Q : Qualification_Type) is
      record
         Bonus : Boolean;
         case Q is
            when Education =>
               Education_Level     : Positive;
            when Skill_Check =>
               Skill               : Concorde.People.Skills.Skill_Type;
               Skill_Check_Level   : Concorde.People.Skills.Skill_Level;
            when Ability_Check =>
               Ability             : Concorde.People.Abilities.Ability_Type;
               Ability_Check_Level :
               Concorde.People.Abilities.Ability_Score_Range;
         end case;
      end record;

   package Qualification_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Qualification);

   type Rank_Record is
      record
         Name   : access String;
         Skills : access Array_Of_Skills;
      end record;

   type Array_Of_Ranks is array (Rank_Index range <>) of Rank_Record;

   type Root_Career_Type is
     new Concorde.Objects.Root_Localised_Object_Type with
      record
         Qualifications : Qualification_Lists.List;
         Advancement    : Qualification_Lists.List;
         Titles         : Boolean;
         Prestige       : Natural;
         Ranks          : access Array_Of_Ranks;
      end record;

   overriding function Object_Database
     (Item : Root_Career_Type)
      return Memor.Memor_Database;

   function Titles
     (Career : Root_Career_Type'Class)
      return Boolean
   is (Career.Titles);

   package Db is
     new Memor.Database
       ("Career", Root_Career_Type, Career_Type);

   overriding function Object_Database
     (Item : Root_Career_Type)
      return Memor.Memor_Database
   is (Db.Get_Database);

   function Exists (Name : String) return Boolean
   is (Db.Exists (Name));

   function Get (Name : String) return Career_Type
   is (Db.Get (Name));

   function Prestige
     (Career : Root_Career_Type'Class)
      return Natural
   is (Career.Prestige);

   function Number_Of_Ranks
     (Career : Root_Career_Type'Class)
      return Rank_Count
   is (Career.Ranks.all'Last);

   function Rank_Name
     (Career : Root_Career_Type'Class;
      Index  : Rank_Index)
      return String
   is (Concorde.Localisation.Local_Name
         (Career.Ranks (Index).Name.all));

   function Rank_Skills
     (Career : Root_Career_Type'Class;
      Index  : Rank_Index)
      return Array_Of_Skills
   is (Career.Ranks (Index).Skills.all);

end Concorde.People.Careers;
