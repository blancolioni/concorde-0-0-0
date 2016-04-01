with Concorde.People.Groups.Db;

package body Concorde.People.Groups is

   ---------
   -- Get --
   ---------

   function Get (Name : String) return Pop_Group is
   begin
      return Db.Get (Name);
   end Get;

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

end Concorde.People.Groups;
