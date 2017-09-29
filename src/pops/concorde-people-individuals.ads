private with Ada.Strings.Unbounded;

private with Memor;
private with Memor.Database;
private with Concorde.People.Skills.Lists;

with Concorde.Agents;
with Concorde.Factions;
with Concorde.Objects;
with Concorde.Trades;

with Concorde.People.Groups;

package Concorde.People.Individuals is

   type Ability_Type is
     (Avarice, Charisma, Empathy, Energy, Health, Honesty, Intelligence);

   type Gender_Type is (Female, Male, None);

   type Score_Range is range 1 .. 30;

   type Root_Individual_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.People.Groups.Affiliation_Interface
     and Concorde.Objects.User_Named_Object_Interface
   with private;

   function Score
     (Individual : Root_Individual_Type'Class;
      Ability    : Ability_Type)
      return Score_Range;

   type Individual_Type is access constant Root_Individual_Type'Class;

   type Updateable_Reference
     (Item : not null access Root_Individual_Type'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Root_Individual_Type'Class)
      return Updateable_Reference;

private

   type Ability_Score_Array is array (Ability_Type) of Score_Range;

   type Root_Individual_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.People.Groups.Affiliation_Interface
     and Concorde.Objects.User_Named_Object_Interface with
      record
         First_Name : Ada.Strings.Unbounded.Unbounded_String;
         Last_Name  : Ada.Strings.Unbounded.Unbounded_String;
         Groups     : Concorde.People.Groups.Affiliation_Vector;
         Skills     : Concorde.People.Skills.Lists.List;
         Gender     : Gender_Type;
         Faction    : Concorde.Factions.Faction_Type;
         Loyalty    : Unit_Real;
         Scores     : Ability_Score_Array;
      end record;

   overriding function Object_Database
     (Item : Root_Individual_Type)
      return Memor.Memor_Database;

   overriding function Class_Name
     (Individual : Root_Individual_Type) return String
   is ("individual");

   overriding function Affiliation
     (Individual : Root_Individual_Type;
      Group      : Concorde.People.Groups.Pop_Group)
      return Concorde.People.Groups.Affiliation_Range
   is (Individual.Groups.Get_Affiliation_Range (Group));

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

   overriding function Variable_Reference
     (Individual : not null access constant Root_Individual_Type)
      return access Concorde.Agents.Root_Agent_Type'Class
   is (Individual.Update.Item);

   function Score
     (Individual : Root_Individual_Type'Class;
      Ability    : Ability_Type)
      return Score_Range
   is (Individual.Scores (Ability));

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
