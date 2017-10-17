package Concorde.Quantities is

   type Quantity_Type is private;

   function Zero return Quantity_Type;
   function Unit return Quantity_Type;

   function To_Quantity (Value : Real) return Quantity_Type;
   function To_Real (Value : Quantity_Type) return Real;
   function To_Natural (Value : Quantity_Type) return Natural;

   function Image (Item : Quantity_Type) return String;
   function Value (Image : String) return Quantity_Type;

   function "*" (Left, Right : Quantity_Type) return Quantity_Type;
   function "/" (Left, Right : Quantity_Type) return Quantity_Type;
   function "+" (Left, Right : Quantity_Type) return Quantity_Type;
   function "-" (Left, Right : Quantity_Type) return Quantity_Type;
   function "<" (Left, Right : Quantity_Type) return Boolean;
   function ">" (Left, Right : Quantity_Type) return Boolean;
   function "<=" (Left, Right : Quantity_Type) return Boolean;
   function ">=" (Left, Right : Quantity_Type) return Boolean;

   function Min (Left, Right : Quantity_Type) return Quantity_Type;
   function Max (Left, Right : Quantity_Type) return Quantity_Type;

   function "abs" (X : Quantity_Type) return Quantity_Type;

   function Around (X : Quantity_Type) return Quantity_Type;
   --  X +/- 10%

   function Scale
     (X : Quantity_Type;
      Factor : Real)
      return Quantity_Type;

   function Scale_Down
     (Value       : Quantity_Type;
      Numerator   : Quantity_Type;
      Denominator : Quantity_Type)
      return Quantity_Type;

private

   type Quantity_Type is range 0 .. 9_999_999;

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
