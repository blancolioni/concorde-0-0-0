with Concorde.People.Groups.Db;

package body Concorde.People.Groups is

   ---------
   -- Get --
   ---------

   function Get (Name : String) return Pop_Group is
   begin
      return Db.Get (Name);
   end Get;

   -------------------------
   -- Initial_Cash_Factor --
   -------------------------

   function Initial_Cash_Factor
     (Group : Root_Pop_Group'Class)
      return Natural
   is
   begin
      return Group.Initial_Cash_Factor;
   end Initial_Cash_Factor;

   ------------------
   -- Middle_Class --
   ------------------

   function Middle_Class return Pop_Group is
   begin
      return Get ("middle_class");
   end Middle_Class;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Item : Root_Pop_Group)
      return Memor.Root_Database_Type'Class
   is
      pragma Unreferenced (Item);
   begin
      return Db.Get_Database;
   end Object_Database;

   ----------
   -- Poor --
   ----------

   function Poor return Pop_Group is
   begin
      return Get ("poor");
   end Poor;

   -----------------------
   -- Preferred_Quality --
   -----------------------

   function Preferred_Quality
     (Group : Root_Pop_Group'Class)
      return Concorde.Commodities.Commodity_Quality
   is
   begin
      return Group.Preferred_Quality;
   end Preferred_Quality;

   ----------
   -- Rich --
   ----------

   function Rich return Pop_Group is
   begin
      return Get ("rich");
   end Rich;

   ------------------
   -- Wealth_Group --
   ------------------

   function Wealth_Group
     (Affiliator : Affiliation_Interface'Class)
      return Pop_Group
   is
   begin
      if Affiliator.Poor then
         return Poor;
      elsif Affiliator.Middle_Class then
         return Middle_Class;
      elsif Affiliator.Rich then
         return Rich;
      else
         raise Constraint_Error with
           "affiliator has no wealth group";
      end if;
   end Wealth_Group;

end Concorde.People.Groups;
