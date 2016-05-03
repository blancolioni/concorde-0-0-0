with Concorde.Terrain.Db;

package body Concorde.Terrain is

   ---------
   -- Get --
   ---------

   function Get (Id : String) return Terrain_Type is
   begin
      return Db.Get (Id);
   end Get;

   --------------
   -- Is_Water --
   --------------

   function Is_Water (Terrain : Root_Terrain_Type'Class) return Boolean is
   begin
      return Terrain.Is_Water;
   end Is_Water;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Terrain : Root_Terrain_Type)
      return Memor.Root_Database_Type'Class
   is
      pragma Unreferenced (Terrain);
   begin
      return Db.Get_Database;
   end Object_Database;

   --------------------
   -- Text_Character --
   --------------------

   function Text_Character
     (Terrain : Root_Terrain_Type'Class)
      return Character
   is
   begin
      return Terrain.Text_Character;
   end Text_Character;

end Concorde.Terrain;
