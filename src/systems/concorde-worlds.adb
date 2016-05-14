with Concorde.Worlds.Db;

package body Concorde.Worlds is

   --------------
   -- Category --
   --------------

   function Category
     (World : Root_World_Type'Class)
      return World_Category
   is
   begin
      return World.Category;
   end Category;

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

   ----------------
   -- Set_Height --
   ----------------

   overriding procedure Set_Height
     (Layout          : in out World_Layout_Type;
      Tile_Index      : Positive;
      Height          : Positive)
   is
   begin
      Layout.Sectors (Tile_Index).Height :=
        Height_Range (Height + Min_Height - 1);
   end Set_Height;

end Concorde.Worlds;
