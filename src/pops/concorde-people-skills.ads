private with Memor.Database;

with Memor;

with WL.Money;
with Concorde.Objects;

with Concorde.Commodities;
with Concorde.People.Groups;

package Concorde.People.Skills is

   type Root_Pop_Skill is
     new Concorde.Objects.Root_Localised_Object_Type with private;

   function Base_Pay
     (Skill : Root_Pop_Skill'Class)
      return WL.Money.Price_Type;

   function Commodity
     (Skill : Root_Pop_Skill'Class)
      return Concorde.Commodities.Commodity_Type;

   function Wealth_Group
     (Skill : Root_Pop_Skill'Class)
      return Concorde.People.Groups.Pop_Group;

   type Pop_Skill is access constant Root_Pop_Skill'Class;

   function Get (Name : String) return Pop_Skill;

private

   type Root_Pop_Skill is
     new Concorde.Objects.Root_Localised_Object_Type with
      record
         Base_Pay     : WL.Money.Price_Type;
         Wealth_Group : Concorde.People.Groups.Pop_Group;
         Commodity    : Concorde.Commodities.Commodity_Type;
      end record;

   overriding function Object_Database
     (Item : Root_Pop_Skill)
      return Memor.Memor_Database;

   package Db is
     new Memor.Database
       ("skill", Root_Pop_Skill, Pop_Skill);

end Concorde.People.Skills;
