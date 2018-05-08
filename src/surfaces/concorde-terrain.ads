private with Memor;
private with Memor.Database;

with Lui.Colors;

with Concorde.Objects;

package Concorde.Terrain is

   type Root_Terrain_Type is
     new Concorde.Objects.Root_Localised_Object_Type
   with private;

   function Is_Water (Terrain : Root_Terrain_Type'Class) return Boolean;

   function Color
     (Terrain : Root_Terrain_Type'Class)
      return Lui.Colors.Color_Type;

   function Text_Character
     (Terrain : Root_Terrain_Type'Class)
      return Character;

   type Terrain_Type is access constant Root_Terrain_Type'Class;

   function Get (Id : String) return Terrain_Type;

   function Mountain return Terrain_Type;

private

   type Root_Terrain_Type is
     new Concorde.Objects.Root_Localised_Object_Type with
      record
         Is_Water       : Boolean;
         Text_Character : Character;
         Color         : Lui.Colors.Color_Type;
      end record;

   overriding function Object_Database
     (Terrain : Root_Terrain_Type)
      return Memor.Memor_Database;

   package Db is
     new Memor.Database
       ("terrain", Root_Terrain_Type, Terrain_Type);

end Concorde.Terrain;
