with Concorde.Worlds;

package Concorde.Terrain.Surface_Maps is

   type Terrain_Frequency_Record is
      record
         Terrain   : Terrain_Type;
         Frequency : Unit_Real;
      end record;

   type Surface_Map is array (Positive range <>) of Terrain_Frequency_Record;

   function Get_Surface_Map
     (Category       : Concorde.Worlds.World_Category;
      Water_Coverage : Unit_Real)
      return Surface_Map;

end Concorde.Terrain.Surface_Maps;
