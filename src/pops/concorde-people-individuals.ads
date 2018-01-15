private with Ada.Strings.Unbounded;

private with Memor;
private with Memor.Database;

with Concorde.Agents;
with Concorde.Factions;
with Concorde.Objects;
with Concorde.Trades;

with Concorde.Calendar;

with Concorde.People.Genetics;
with Concorde.People.Groups;
with Concorde.People.Skills;

package Concorde.People.Individuals is

   type Ability_Type is
     (Avarice, Charisma, Empathy, Energy,
      Health, Honesty, Intelligence, Strength);

   type Ability_Score_Range is range 1 .. 30;

--     type Skill_Type is
--       (Administration, Advocate, Broker, Command_Ship,
--        Communications, Computers, Diplomacy, Engineering,
--        Influence, Leadership, Navigation, Sensors,
--        Tactics, Trade);
--
--     type Skill_Score_Range is range 0 .. 10;

   type Gender_Type is (Female, Male, None);

   type Root_Individual_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Factions.Citizen_Interface
     and Concorde.Objects.User_Named_Object_Interface
     and Concorde.People.Skills.Has_Skills_Interface
   with private;

   overriding function Level
     (Individual : Root_Individual_Type;
      Skill      : not null access constant
        Concorde.People.Skills.Root_Skill_Type'Class)
      return Concorde.People.Skills.Skill_Level;

   function Ability_Score
     (Individual : Root_Individual_Type'Class;
      Ability    : Ability_Type)
      return Ability_Score_Range;

   function Last_Name (Individual : Root_Individual_Type'Class)
                       return String;

   function First_Name (Individual : Root_Individual_Type'Class)
                        return String;

   function Full_Name (Individual : Root_Individual_Type'Class)
                       return String;

   function Age
     (Individual : Root_Individual_Type'Class)
      return Natural;

   type Individual_Type is access constant Root_Individual_Type'Class;

   type Updateable_Reference
     (Item : not null access Root_Individual_Type'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Root_Individual_Type'Class)
      return Updateable_Reference;

private

   type Ability_Score_Array is array (Ability_Type) of Ability_Score_Range;

   type Root_Individual_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Factions.Citizen_Interface
     and Concorde.Objects.User_Named_Object_Interface
     and Concorde.People.Skills.Has_Skills_Interface with
      record
         First_Name  : Ada.Strings.Unbounded.Unbounded_String;
         Last_Name   : Ada.Strings.Unbounded.Unbounded_String;
         DNA         : Concorde.People.Genetics.Genome;
         Gender      : Gender_Type;
         Birth       : Concorde.Calendar.Time;
         Death       : Concorde.Calendar.Time;
         Alive       : Boolean;
         Faction     : Concorde.Factions.Faction_Type;
         Citizenship : Concorde.Factions.Faction_Type;
         Group       : Concorde.People.Groups.Pop_Group;
         Loyalty     : Unit_Real;
         Abilities   : Ability_Score_Array;
         Skills      : Concorde.People.Skills.Skill_Set;
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

   overriding function Level
     (Individual : Root_Individual_Type;
      Skill      : not null access constant
        Concorde.People.Skills.Root_Skill_Type'Class)
      return Concorde.People.Skills.Skill_Level
   is (Individual.Skills.Level (Skill));

   function Ability_Score
     (Individual : Root_Individual_Type'Class;
      Ability    : Ability_Type)
      return Ability_Score_Range
   is (Individual.Abilities (Ability));

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
