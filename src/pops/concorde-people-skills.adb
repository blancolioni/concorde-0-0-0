with Concorde.People.Skills.Db;

package body Concorde.People.Skills is

   --------------
   -- Base_Pay --
   --------------

   function Base_Pay
     (Skill : Root_Pop_Skill'Class)
      return Concorde.Money.Price_Type
   is
   begin
      return Skill.Base_Pay;
   end Base_Pay;

   ---------------
   -- Commodity --
   ---------------

   function Commodity
     (Skill : Root_Pop_Skill'Class)
      return Concorde.Commodities.Commodity_Type
   is
   begin
      return Skill.Commodity;
   end Commodity;

   ---------
   -- Get --
   ---------

   function Get (Name : String) return Pop_Skill is
   begin
      return Db.Get (Name);
   end Get;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Item : Root_Pop_Skill)
      return Memor.Root_Database_Type'Class
   is
      pragma Unreferenced (Item);
   begin
      return Db.Get_Database;
   end Object_Database;

end Concorde.People.Skills;
