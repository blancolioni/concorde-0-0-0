private with Ada.Containers.Vectors;
private with Ada.Containers.Indefinite_Vectors;

package Concorde.Voronoi_Diagrams is

   type Voronoi_Diagram is tagged limited private;

   procedure Clear (Item : in out Voronoi_Diagram'Class);

   procedure Add_Point
     (To   : in out Voronoi_Diagram'Class;
      X, Y : Real);

   procedure Generate
     (Diagram : in out Voronoi_Diagram'Class);

   function Polygon_Count
     (Item : Voronoi_Diagram'Class)
      return Natural;

   function Vertex_Count
     (Item          : Voronoi_Diagram'Class;
      Polygon_Index : Positive)
      return Natural;

   function Vertex_X
     (Item : Voronoi_Diagram'Class;
      Polygon_Index : Positive;
      Vertex_Index  : Positive)
      return Real;

   function Vertex_Y
     (Item : Voronoi_Diagram'Class;
      Polygon_Index : Positive;
      Vertex_Index  : Positive)
      return Real;

private

   type Voronoi_Point is
      record
         X, Y : Real;
      end record;

   package Point_Vectors is
     new Ada.Containers.Vectors (Positive, Voronoi_Point);

   type Voronoi_Site is
      record
         Point      : Voronoi_Point;
         Site_Index : Positive;
      end record;

   package Site_Vectors is
     new Ada.Containers.Vectors (Positive, Voronoi_Site);

   type Point_Index_Array is array (Positive range <>) of Positive;

   type Voronoi_Polygon (Count : Natural) is
      record
         Vertices : Point_Index_Array (1 .. Count);
      end record;

   package Polygon_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Voronoi_Polygon);

   type Voronoi_Diagram is tagged limited
      record
         Sites        : Site_Vectors.Vector;
         Min_X, Min_Y : Real := Real'Last;
         Max_X, Max_Y : Real := Real'First;
         Diagram_Pts  : Point_Vectors.Vector;
         Diagram      : Polygon_Vectors.Vector;
      end record;

end Concorde.Voronoi_Diagrams;
