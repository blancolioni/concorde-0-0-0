with Concorde.Installations.Db;

package body Concorde.Installations is

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Item : Root_Installation_Type)
      return Memor.Root_Database_Type'Class
   is
      pragma Unreferenced (Item);
   begin
      return Db.Get_Database;
   end Object_Database;

end Concorde.Installations;
