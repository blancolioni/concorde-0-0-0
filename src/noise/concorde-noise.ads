with Xi;

package Concorde.Noise is

   type Dimension_Range is range 1 .. 4;

   type Noise_Vector is array (Dimension_Range range <>) of Xi.Xi_Float;

   type Perlin_Noise (Dimension_Count : Dimension_Range) is tagged private;

   procedure Reset (Noise : in out Perlin_Noise'Class;
                    Initiator       : Integer);

   function Get
     (Noise      : Perlin_Noise'Class;
      Coordinate : Noise_Vector)
      return Xi.Xi_Signed_Unit_Float
     with Pre => Coordinate'First = 1
     and then Coordinate'Last = Noise.Dimension_Count;

private

   type Map_Index_Type is mod 256;

   type Map_Type is array (Map_Index_Type) of Map_Index_Type;

   type Dimension_Index_Buffer is
     array (Dimension_Range range <>) of Map_Index_Type;

   type Dimension_Float_Buffer is
     array (Dimension_Range range <>) of Xi.Xi_Float;

   type Dimension_Buffer_Access is access Dimension_Float_Buffer;

   type Buffer_Type is array (Map_Index_Type) of Dimension_Buffer_Access;

   type Perlin_Noise (Dimension_Count : Dimension_Range) is tagged
      record
         Map             : Map_Type;
         Buffer          : Buffer_Type;
      end record;

   type Index_Record is
      record
         Index     : Map_Index_Type;
         Remainder : Xi.Xi_Float;
         Cubic     : Xi.Xi_Float;
      end record;

   type Index_Record_Array is
     array (Dimension_Range range <>) of Index_Record;

   function Lattice
     (Noise : Perlin_Noise;
      Indices : Dimension_Index_Buffer;
      Remainders : Dimension_Float_Buffer)
      return Xi.Xi_Float
     with Pre => Indices'First = 1
     and then Remainders'First = 1
     and then Indices'Last = Noise.Dimension_Count
     and then Remainders'Last = Noise.Dimension_Count;

end Concorde.Noise;
