with Xi;

package Concorde is

   use type Xi.Xi_Float;

   subtype Real is Xi.Xi_Float;

   subtype Unit_Real is Real range 0.0 .. 1.0;
   subtype Signed_Unit_Real is Real range -1.0 .. 1.0;

   subtype Non_Negative_Real is Real range 0.0 .. Real'Last;

   type Point_Type is
      record
         X, Y : Real;
      end record;

   type Size_Type is
      record
         X, Y, Z : Positive;
      end record;

end Concorde;
