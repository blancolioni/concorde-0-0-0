with Xi;

with Concorde.Noise;

package Concorde.Brownian_Noise is

   type Octave_Range is range 1 .. 128;

   type Brownian_Noise_Type
     (Dimension_Count : Concorde.Noise.Dimension_Range)
   is new Concorde.Noise.Perlin_Noise with private;

   procedure Reset
     (Noise       : in out Brownian_Noise_Type'Class;
      Initiator   : Integer;
      Roughness   : Xi.Xi_Unit_Float;
      Lacunarity  : Xi.Xi_Float);

   function Get
     (Noise      : Brownian_Noise_Type'Class;
      Coordinate : Concorde.Noise.Noise_Vector;
      Octave     : Xi.Xi_Float)
      return Xi.Xi_Signed_Unit_Float
     with Pre => Concorde.Noise."=" (Coordinate'First, 1)
     and then Concorde.Noise."=" (Coordinate'Last, Noise.Dimension_Count);

private

   type Exponent_Array is array (Octave_Range) of Xi.Xi_Float;

   type Brownian_Noise_Type
     (Dimension_Count : Concorde.Noise.Dimension_Range)
   is new Concorde.Noise.Perlin_Noise (Dimension_Count) with
      record
         Roughness  : Xi.Xi_Unit_Float;
         Lacunarity : Xi.Xi_Float;
         Exponents  : Exponent_Array;
      end record;

end Concorde.Brownian_Noise;
