private with Memor;

with Concorde.Objects;

package Concorde.Terrain is

   type Root_Terrain_Type is
     new Concorde.Objects.Root_Named_Object_Type
   with private;

   function Is_Water (Terrain : Root_Terrain_Type'Class) return Boolean;

   function Text_Character
     (Terrain : Root_Terrain_Type'Class)
      return Character;

   type Terrain_Type is access constant Root_Terrain_Type'Class;

   function Get (Id : String) return Terrain_Type;

private

   type Root_Terrain_Type is
     new Concorde.Objects.Root_Named_Object_Type with
      record
         Is_Water       : Boolean;
         Text_Character : Character;
      end record;

   overriding function Object_Database
     (Terrain : Root_Terrain_Type)
      return Memor.Root_Database_Type'Class;

end Concorde.Terrain;
