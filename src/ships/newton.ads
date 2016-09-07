with Xi.Float_Arrays;

package Newton is

   subtype Real is Xi.Xi_Float;
   subtype Unit_Real is Real range 0.0 .. 1.0;

   package Matrices renames Xi.Float_Arrays;

   subtype Vector_3 is Matrices.Real_Vector (1 .. 3);

   subtype Matrix_3 is Matrices.Real_Matrix (1 .. 3, 1 .. 3);

   subtype Vector_4 is Matrices.Real_Vector (1 .. 4);

   subtype Matrix_4 is Matrices.Real_Matrix (1 .. 4, 1 .. 4);

end Newton;
