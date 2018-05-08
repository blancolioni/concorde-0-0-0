package body Concorde.Terrain is

   Local_Mountain : Terrain_Type;

   ------------
   -- Color --
   ------------

   function Color
     (Terrain : Root_Terrain_Type'Class)
      return Lui.Colors.Color_Type
   is
   begin
      return Terrain.Color;
   end Color;

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

   --------------
   -- Mountain --
   --------------

   function Mountain return Terrain_Type is
   begin
      if Local_Mountain = null then
         Local_Mountain := Db.Get ("mountains");
      end if;
      return Local_Mountain;
   end Mountain;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Terrain : Root_Terrain_Type)
      return Memor.Memor_Database
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
