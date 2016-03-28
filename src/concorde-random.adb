with Ada.Numerics.Float_Random;

package body Concorde.Random is

   Gen : Ada.Numerics.Float_Random.Generator;

   -----------------
   -- Unit_Random --
   -----------------

   function Unit_Random return Unit_Real is
   begin
      return Unit_Real (Ada.Numerics.Float_Random.Random (Gen));
   end Unit_Random;

end Concorde.Random;
