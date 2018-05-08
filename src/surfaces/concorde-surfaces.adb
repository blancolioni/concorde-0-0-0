with Ada.Numerics;

with Concorde.Elementary_Functions;

package body Concorde.Surfaces is

   Max_Depth : constant := 5;

   function Cross
     (Left, Right : Xi.Matrices.Vector_3)
      return Xi.Matrices.Vector_3;

   type Vertex_Index_Map is
      record
         Index_1      : Surface_Tile_Index;
         Index_2      : Surface_Tile_Index;
         Centre_Index : Surface_Tile_Index;
      end record;

   package Vertex_Map_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Vertex_Index_Map);

   --------------------
   -- Create_Surface --
   --------------------

   function Create_Surface
     (Tile_Area : Non_Negative_Real)
      return Surface_Type
   is
      use Xi.Matrices, Xi.Float_Arrays;

      Pi : constant := Ada.Numerics.Pi;

      Surface : constant Surface_Type := new Root_Surface_Type;
      Vs      : Vertex_Vectors.Vector renames Surface.Vertices;

      X    : constant := 0.525731112119133606;
      Z    : constant := 0.850650808352039932;

      Vertex_Data                                    : constant array
        (Surface_Tile_Index range 1 .. 12)
        of Xi.Matrices.Vector_3 :=
        ((-X, 0.0, Z), (X, 0.0, Z), (-X, 0.0, -Z), (X, 0.0, -Z),
         (0.0, Z, X), (0.0, Z, -X), (0.0, -Z, X), (0.0, -Z, -X),
         (Z, X, 0.0), (-Z, X, 0.0), (Z, -X, 0.0), (-Z, -X, 0.0));
      Triangles   : constant array (1 .. 20, 1 .. 3) of Surface_Tile_Index :=
        ((2, 5, 1), (5, 10, 1), (5, 6, 10), (9, 6, 5), (2, 9, 5),
         (2, 11, 9), (11, 4, 9), (9, 4, 6), (4, 3, 6), (4, 8, 3),
         (4, 11, 8), (11, 7, 8), (7, 12, 8), (7, 1, 12), (7, 2, 1),
         (11, 2, 7), (12, 1, 10), (3, 12, 10), (6, 3, 10), (12, 3, 8));

      Vertex_Map : Vertex_Map_Lists.List;

      procedure Add_Neighbour
        (Tile      : in out Vertex_Record;
         Neighbour : in Surface_Tile_Index);

      procedure Add_Vertex
        (Tile      : Surface_Tile_Index;
         Vertex    : Tile_Vertex_Index);

      procedure Subdivide
        (Index_1    : Surface_Tile_Index;
         Index_2    : Surface_Tile_Index;
         Index_3    : Surface_Tile_Index;
         Depth      : Natural);

      function Divide_Vertices
        (Index_1 : Surface_Tile_Index;
         Index_2 : Surface_Tile_Index)
         return Surface_Tile_Index;

      -------------------
      -- Add_Neighbour --
      -------------------

      procedure Add_Neighbour
        (Tile      : in out Vertex_Record;
         Neighbour : in Surface_Tile_Index)
      is
      begin
         Tile.Neighbour_Count := Tile.Neighbour_Count + 1;
         Tile.Neighbours (Tile.Neighbour_Count) := Neighbour;
      end Add_Neighbour;

      ----------------
      -- Add_Vertex --
      ----------------

      procedure Add_Vertex
        (Tile      : Surface_Tile_Index;
         Vertex    : Tile_Vertex_Index)
      is
         use Vertex_Lists;
         Edge : Vertex_Lists.List renames Surface.Tile_Edges (Tile);
         Norm : constant Vector_3 := Surface.Vertices.Element (Tile).Position;
         V1   : constant Vector_3 := Surface.Tile_Vertices.Element (Vertex);
         Position : Cursor := Edge.Last;
      begin
         while Has_Element (Position) loop
            declare
               V2 : constant Vector_3 :=
                      Surface.Tile_Vertices (Element (Position));
            begin
               exit when Norm * Cross (V1 - Norm, V2 - Norm) < 0.0;
               Previous (Position);
            end;
         end loop;

         if Has_Element (Position) then
            Next (Position);
            if Has_Element (Position) then
               Edge.Insert (Position, Vertex);
            else
               Edge.Append (Vertex);
            end if;
         else
            Edge.Insert (Edge.First, Vertex);
         end if;

      end Add_Vertex;

      ---------------------
      -- Divide_Vertices --
      ---------------------

      function Divide_Vertices
        (Index_1 : Surface_Tile_Index;
         Index_2 : Surface_Tile_Index)
         return Surface_Tile_Index
      is
      begin
         for Element of Vertex_Map loop
            if (Element.Index_1 = Index_1 and then Element.Index_2 = Index_2)
              or else
                (Element.Index_2 = Index_1 and then Element.Index_1 = Index_2)
            then
               return Element.Centre_Index;
            end if;
         end loop;

         declare
            V1  : constant Vector_3 :=
                    Vs.Element (Index_1).Position;
            V2  : constant Vector_3 :=
                    Vs.Element (Index_2).Position;
            V12 : Vector_3 :=  (V1 + V2) / 2.0;
         begin
            V12 := V12 / abs V12;
            Vs.Append ((V12, 0, (others => 1)));
            Vertex_Map.Append ((Index_1, Index_2, Vs.Last_Index));
            return Vs.Last_Index;
         end;
      end Divide_Vertices;

      ---------------
      -- Subdivide --
      ---------------

      procedure Subdivide
        (Index_1 : Surface_Tile_Index;
         Index_2 : Surface_Tile_Index;
         Index_3 : Surface_Tile_Index;
         Depth   : Natural)
      is
      begin
         if Depth = 0 then
            declare
               Tile_1 : Vertex_Record renames Vs (Index_1);
               Tile_2 : Vertex_Record renames Vs (Index_2);
               Tile_3 : Vertex_Record renames Vs (Index_3);
            begin
               Add_Neighbour (Tile_1, Index_2);
               Add_Neighbour (Tile_2, Index_3);
               Add_Neighbour (Tile_3, Index_1);
               Surface.Triangles.Append ((Index_1, Index_2, Index_3));
            end;
         else
            declare
               Index_12 : constant Surface_Tile_Index :=
                            Divide_Vertices (Index_1, Index_2);
               Index_23 : constant Surface_Tile_Index :=
                            Divide_Vertices (Index_2, Index_3);
               Index_31 : constant Surface_Tile_Index :=
                            Divide_Vertices (Index_3, Index_1);
            begin
               Subdivide (Index_1, Index_12, Index_31, Depth - 1);
               Subdivide (Index_2, Index_23, Index_12, Depth - 1);
               Subdivide (Index_3, Index_31, Index_23, Depth - 1);
               Subdivide (Index_12, Index_23, Index_31, Depth - 1);
            end;
         end if;
      end Subdivide;

      Required_Depth : Natural := 0;
      Current_Area   : Non_Negative_Real :=
                         4.0 / 3.0 * Pi ** 2 / 12.0;
      Total_Tiles    : Positive := 12;
   begin
      for V of Vertex_Data loop
         Vs.Append ((V, 0, (others => 1)));
      end loop;

      while Current_Area > Tile_Area loop
         Required_Depth := Required_Depth + 1;
         Total_Tiles := Total_Tiles * 6;
         Current_Area := Current_Area / 6.0;
      end loop;

      for I in Triangles'Range (1) loop
         Subdivide (Triangles (I, 1),
                    Triangles (I, 2),
                    Triangles (I, 3),
                    Natural'Min (Required_Depth, Max_Depth));
      end loop;

      for V of Surface.Vertices loop
         Surface.Tile_Edges.Append (Vertex_Lists.Empty_List);
      end loop;

      for Triangle of Surface.Triangles loop
         declare
            Sum : constant Vector_3 :=
                    Surface.Vertices.Element (Triangle.V1).Position
                    + Surface.Vertices.Element (Triangle.V2).Position
                    + Surface.Vertices.Element (Triangle.V3).Position;
            V   : constant Vector_3 := Sum / abs Sum;
         begin
            Surface.Tile_Vertices.Append (V);
            Add_Vertex (Triangle.V1, Surface.Tile_Vertices.Last_Index);
            Add_Vertex (Triangle.V2, Surface.Tile_Vertices.Last_Index);
            Add_Vertex (Triangle.V3, Surface.Tile_Vertices.Last_Index);
         end;
      end loop;

      return Surface;

   end Create_Surface;

   -----------
   -- Cross --
   -----------

   function Cross
     (Left, Right : Xi.Matrices.Vector_3)
      return Xi.Matrices.Vector_3
   is
   begin
      return (Left (2) * Right (3) - Left (3) * Right (2),
              Left (3) * Right (1) - Left (1) * Right (3),
              Left (1) * Right (2) - Left (2) * Right (1));
   end Cross;

   --------------
   -- Get_Tile --
   --------------

   function Get_Tile
     (Relative_Latitude  : Signed_Unit_Real;
      Relative_Longitude : Unit_Real)
      return Surface_Tile_Index
   is
      pragma Unreferenced (Relative_Latitude);
      pragma Unreferenced (Relative_Longitude);
   begin
      return 1;
   end Get_Tile;

   --------------
   -- Latitude --
   --------------

   function Latitude
     (Surface : Root_Surface_Type'Class;
      Tile    : Surface_Tile_Index)
      return Real
   is
      use Concorde.Elementary_Functions;
   begin
      return Arcsin (Surface.Vertices.Element (Tile).Position (2),
                     Cycle => 360.0);
   end Latitude;

   ---------------
   -- Longitude --
   ---------------

   function Longitude
     (Surface : Root_Surface_Type'Class;
      Tile    : Surface_Tile_Index)
      return Real
   is
      use Concorde.Elementary_Functions;
      V : constant Xi.Matrices.Vector_3 :=
            Surface.Vertices.Element (Tile).Position;
   begin
      return Arctan (V (3), V (1), 360.0);
   end Longitude;

   ---------------
   -- Neighbour --
   ---------------

   function Neighbour
     (Surface : Root_Surface_Type'Class;
      Tile    : Surface_Tile_Index;
      Index   : Tile_Neighbour_Index)
      return Surface_Tile_Index
   is
   begin
      return Surface.Vertices.Element (Tile).Neighbours (Index);
   end Neighbour;

   ---------------------
   -- Neighbour_Count --
   ---------------------

   function Neighbour_Count
     (Surface : Root_Surface_Type'Class;
      Tile    : Surface_Tile_Index)
      return Tile_Neighbour_Count
   is
   begin
      return Surface.Vertices.Element (Tile).Neighbour_Count;
   end Neighbour_Count;

   -------------------
   -- Tile_Boundary --
   -------------------

   function Tile_Boundary
     (Surface : Root_Surface_Type'Class;
      Tile    : Surface_Tile_Index)
      return Tile_Vertex_Array
   is
      Edge : Vertex_Lists.List renames Surface.Tile_Edges (Tile);
   begin
      return Boundary : Tile_Vertex_Array
        (1 .. Tile_Neighbour_Count (Edge.Length))
      do
         declare
            Count : Tile_Neighbour_Count := 0;
         begin
            for Index of Edge loop
               Count := Count + 1;
               Boundary (Count) := Surface.Tile_Vertices (Index);
            end loop;
         end;
      end return;
   end Tile_Boundary;

   -----------------
   -- Tile_Centre --
   -----------------

   function Tile_Centre
     (Surface : Root_Surface_Type'Class;
      Tile    : Surface_Tile_Index)
      return Xi.Matrices.Vector_3
   is
   begin
      return Surface.Vertices.Element (Tile).Position;
   end Tile_Centre;

   ----------------
   -- Tile_Count --
   ----------------

   function Tile_Count
     (Surface : Root_Surface_Type'Class)
      return Surface_Tile_Count
   is
   begin
      return Surface.Vertices.Last_Index;
   end Tile_Count;

end Concorde.Surfaces;
