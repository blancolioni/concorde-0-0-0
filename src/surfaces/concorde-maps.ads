with Ada.Containers.Doubly_Linked_Lists;

package Concorde.Maps is

   type Tile_Layout_Interface is limited interface;

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

   package List_Of_Tiles is
     new Ada.Containers.Doubly_Linked_Lists (Positive);

   procedure Scan_Tiles
     (Layout : Tile_Layout_Interface'Class;
      Match  : not null access
        function (Tile_Index : Positive)
      return Boolean;
      Result : out List_Of_Tiles.List);

   procedure External_Border
     (Layout : Tile_Layout_Interface'Class;
      Tiles  : List_Of_Tiles.List;
      Border : out List_Of_Tiles.List);

   procedure Internal_Border
     (Layout : Tile_Layout_Interface'Class;
      Tiles  : List_Of_Tiles.List;
      Border : out List_Of_Tiles.List);

   procedure Expand_Border
     (Layout : Tile_Layout_Interface'Class;
      Tiles  : in out List_Of_Tiles.List);

   procedure Expand_Border
     (Layout : Tile_Layout_Interface'Class;
      Tiles  : in out List_Of_Tiles.List;
      Match  : not null access
        function (Tile_Index : Positive) return Boolean);

end Concorde.Maps;
