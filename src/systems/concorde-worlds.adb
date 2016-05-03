with Concorde.Worlds.Db;

package body Concorde.Worlds is

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (World : Root_World_Type)
      return Memor.Root_Database_Type'Class
   is
      pragma Unreferenced (World);
   begin
      return Db.Get_Database;
   end Object_Database;

end Concorde.Worlds;
