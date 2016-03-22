with Lui;

package Concorde is

   subtype Real is Lui.Real;

   subtype Unit_Real is Real range 0.0 .. 1.0;

   subtype Non_Negative_Real is Real range 0.0 .. Real'Last;

   type Point_Type is
      record
         X, Y : Real;
      end record;

end Concorde;
