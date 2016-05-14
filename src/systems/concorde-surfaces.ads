with Ada.Numerics.Long_Real_Arrays;
private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;

package Concorde.Surfaces is

   Max_Surface_Tiles : constant := 60_000;

   Min_Tile_Neighbours : constant := 5;
   Max_Tile_Neighbours : constant := 7;

   type Surface_Tile_Count is range 0 .. Max_Surface_Tiles;
   subtype Surface_Tile_Index is
     Surface_Tile_Count range 1 .. Surface_Tile_Count'Last;

   type Tile_Neighbour_Count is range 0 .. Max_Tile_Neighbours;

   subtype Tile_Neighbour_Index is
     Tile_Neighbour_Count range 1 .. Tile_Neighbour_Count'Last;

   type Array_Of_Tile_Neighbours is
     array (Tile_Neighbour_Index range <>) of Surface_Tile_Index;

   type Root_Surface_Type is tagged private;

   function Tile_Count
     (Surface : Root_Surface_Type'Class)
      return Surface_Tile_Count;

   function Neighbour_Count
     (Surface : Root_Surface_Type'Class;
      Tile    : Surface_Tile_Index)
      return Tile_Neighbour_Count;

   function Neighbour
     (Surface : Root_Surface_Type'Class;
      Tile    : Surface_Tile_Index;
      Index   : Tile_Neighbour_Index)
      return Surface_Tile_Index;

   function Get_Tile
     (Relative_Latitude  : Signed_Unit_Real;
      Relative_Longitude : Unit_Real)
      return Surface_Tile_Index;

   subtype Vector_3 is
     Ada.Numerics.Long_Real_Arrays.Real_Vector (1 .. 3);

   type Tile_Vertex_Array is
     array (Tile_Neighbour_Index range <>) of Vector_3;

   function Tile_Centre
     (Surface : Root_Surface_Type'Class;
      Tile    : Surface_Tile_Index)
      return Vector_3;

   function Tile_Boundary
     (Surface : Root_Surface_Type'Class;
      Tile    : Surface_Tile_Index)
      return Tile_Vertex_Array;

   type Surface_Type is access all Root_Surface_Type'Class;

   function Create_Surface
     (Tile_Area : Non_Negative_Real)
      return Surface_Type;

private

   type Vertex_Record is
      record
         Position        : Vector_3;
         Neighbour_Count : Tile_Neighbour_Count;
         Neighbours      : Array_Of_Tile_Neighbours (Tile_Neighbour_Index);
      end record;

   package Vertex_Vectors is
     new Ada.Containers.Vectors (Surface_Tile_Index, Vertex_Record);

   type Triangle_Record is
      record
         V1, V2, V3 : Surface_Tile_Index;
      end record;

   type Array_Of_Surface_Tile_Index is
     array (Positive range <>) of Surface_Tile_Index;

   type Tile_Vertex_Index is new Surface_Tile_Index;

   package Tile_Vertex_Vectors is
     new Ada.Containers.Vectors (Tile_Vertex_Index, Vector_3,
                                 Ada.Numerics.Long_Real_Arrays."=");

   package Vertex_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Tile_Vertex_Index);

   package Edge_Vectors is
     new Ada.Containers.Vectors
       (Surface_Tile_Index, Vertex_Lists.List, Vertex_Lists."=");

   package Triangle_Vectors is
     new Ada.Containers.Vectors (Surface_Tile_Index, Triangle_Record);

   type Root_Surface_Type is tagged
      record
         Vertices      : Vertex_Vectors.Vector;
         Triangles     : Triangle_Vectors.Vector;
         Tile_Vertices : Tile_Vertex_Vectors.Vector;
         Tile_Edges    : Edge_Vectors.Vector;
      end record;

end Concorde.Surfaces;
