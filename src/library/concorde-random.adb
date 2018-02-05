with Ada.Numerics.Float_Random;

package body Concorde.Random is

   Gen : Ada.Numerics.Float_Random.Generator;

   -----------
   -- About --
   -----------

   function About
     (Value : Real;
      Variation : Real)
      return Real
   is
   begin
      return Value - Variation + Unit_Random * Variation * 2.0;
   end About;

   -----------------
   -- Unit_Random --
   -----------------

   function Unit_Random return Unit_Real is
   begin
      return Unit_Real (Ada.Numerics.Float_Random.Random (Gen));
   end Unit_Random;

end Concorde.Random;
