package Concorde.Quantities is

   type Quantity is private;

   function Zero return Quantity;
   function Unit return Quantity;

   function To_Quantity (Value : Real) return Quantity;
   function To_Real (Value : Quantity) return Real;
   function To_Natural (Value : Quantity) return Natural;

   function Image (Item : Quantity) return String;
   function Value (Image : String) return Quantity;

   function "*" (Left, Right : Quantity) return Quantity;
   function "/" (Left, Right : Quantity) return Quantity;
   function "+" (Left, Right : Quantity) return Quantity;
   function "-" (Left, Right : Quantity) return Quantity;
   function "<" (Left, Right : Quantity) return Boolean;
   function ">" (Left, Right : Quantity) return Boolean;
   function "<=" (Left, Right : Quantity) return Boolean;
   function ">=" (Left, Right : Quantity) return Boolean;

   function Min (Left, Right : Quantity) return Quantity;
   function Max (Left, Right : Quantity) return Quantity;

   function "abs" (X : Quantity) return Quantity;

   function Around (X : Quantity) return Quantity;
   --  X +/- 10%

   function Scale
     (X : Quantity;
      Factor : Unit_Real)
      return Quantity;

private

   type Quantity is range -2**63 .. 2**63 - 1;

   pragma Import (Intrinsic, "*");
   pragma Import (Intrinsic, "/");
   pragma Import (Intrinsic, "+");
   pragma Import (Intrinsic, "-");
   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">=");
   pragma Import (Intrinsic, "abs");

end Concorde.Quantities;
