private with Ada.Strings.Unbounded;

private with Memor;
private with Memor.Element_Vectors;
private with Concorde.People.Skills.Lists;

with Concorde.Agents;
with Concorde.Objects;
with Concorde.Owners;
with Concorde.Trades;

with Concorde.People.Groups;

package Concorde.People.Individuals is

   type Ability_Type is
     (Charisma, Intelligence, Empathy, Energy, Health);

   type Score_Range is range 1 .. 30;

   type Root_Individual_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.People.Groups.Affiliation_Interface
     and Concorde.Owners.Owner_Interface
     and Concorde.Objects.User_Named_Object_Interface
   with private;

   function Score
     (Individual : Root_Individual_Type'Class;
      Ability    : Ability_Type)
      return Score_Range;

   type Individual_Type is access constant Root_Individual_Type'Class;

private

   package Group_Affiliation_Vectors is
     new Memor.Element_Vectors
       (Concorde.People.Groups.Affiliation_Range, 0.0,
        Concorde.People.Groups."=");

   type Ability_Score_Array is array (Ability_Type) of Score_Range;

   type Root_Individual_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.People.Groups.Affiliation_Interface
     and Concorde.Owners.Owner_Interface
     and Concorde.Objects.User_Named_Object_Interface with
      record
         First_Name : Ada.Strings.Unbounded.Unbounded_String;
         Last_Name  : Ada.Strings.Unbounded.Unbounded_String;
         Groups     : Group_Affiliation_Vectors.Vector;
         Skills     : Concorde.People.Skills.Lists.List;
         Scores     : Ability_Score_Array;
      end record;

   overriding function Object_Database
     (Item : Root_Individual_Type)
      return Memor.Root_Database_Type'Class;

   overriding function Affiliation
     (Individual : Root_Individual_Type;
      Group      : Concorde.People.Groups.Pop_Group)
      return Concorde.People.Groups.Affiliation_Range;

   overriding function Name
     (Individual : Root_Individual_Type)
      return String;

   overriding procedure Set_Name
     (Individual : in out Root_Individual_Type;
      New_Name   : String);

   overriding function Short_Name
     (Item : Root_Individual_Type)
      return String
   is (Item.Name);

   overriding procedure Add_Trade_Offers
     (Item   : not null access constant Root_Individual_Type;
      Market : in out Concorde.Trades.Trade_Interface'Class);

   overriding procedure Before_Market
     (Individual : in out Root_Individual_Type);

end Concorde.People.Individuals;
