package Concorde.Maps is

   type Tile_Layout_Interface is interface;

   function Tile_Count
     (Layout     : Tile_Layout_Interface)
      return Natural
      is abstract;

   function Neighbour_Count
     (Layout     : Tile_Layout_Interface;
      Tile_Index : Positive)
      return Natural
      is abstract;

   function Neighbour
     (Layout          : Tile_Layout_Interface;
      Tile_Index      : Positive;
      Neighbour_Index : Positive)
      return Positive
      is abstract;

   procedure Set_Height
     (Layout     : in out Tile_Layout_Interface;
      Tile_Index : Positive;
      Height     : Positive)
   is abstract;

   type Frequency_Array is array (Positive range <>) of Unit_Real;

end Concorde.Maps;
