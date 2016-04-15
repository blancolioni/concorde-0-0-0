with Memor;

with Concorde.Money;
with Concorde.Objects;

with Concorde.Commodities;

package Concorde.People.Skills is

   type Root_Pop_Skill is
     new Concorde.Objects.Root_Named_Object_Type with private;

   function Base_Pay
     (Skill : Root_Pop_Skill'Class)
      return Concorde.Money.Price_Type;

   function Commodity
     (Skill : Root_Pop_Skill'Class)
      return Concorde.Commodities.Commodity_Type;

   type Pop_Skill is access constant Root_Pop_Skill'Class;

   function Get (Name : String) return Pop_Skill;

private

   type Root_Pop_Skill is
     new Concorde.Objects.Root_Named_Object_Type with
      record
         Pop_Skill_Id : access String;
         Base_Pay     : Concorde.Money.Price_Type;
         Commodity    : Concorde.Commodities.Commodity_Type;
      end record;

   overriding function Object_Database
     (Item : Root_Pop_Skill)
      return Memor.Root_Database_Type'Class;

   overriding function Identifier
     (Item : Root_Pop_Skill)
      return String
   is (Item.Pop_Skill_Id.all);

end Concorde.People.Skills;
