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
      return Group.Initial_Cash;
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

   ----------
   -- Rich --
   ----------

   function Rich return Pop_Group is
   begin
      return Get ("rich");
   end Rich;

end Concorde.People.Groups;
