with Concorde.Quantities;

package Concorde.Money is

   type Money_Type is private;

   function Zero return Money_Type;

   function "+" (Left, Right : Money_Type) return Money_Type;
   function "-" (Left, Right : Money_Type) return Money_Type;

   function "<"  (Left, Right : Money_Type) return Boolean;
   function ">"  (Left, Right : Money_Type) return Boolean;
   function "<=" (Left, Right : Money_Type) return Boolean;
   function ">=" (Left, Right : Money_Type) return Boolean;

   function "abs" (Left : Money_Type) return Money_Type;

   function Adjust (Money    : Money_Type;
                    Factor   : Real)
                   return Money_Type;

   function To_Money (Amount : Real) return Money_Type;
   function To_Real (Amount : Money_Type) return Real;

   function Max (X, Y : Money_Type) return Money_Type;
   function Min (X, Y : Money_Type) return Money_Type;

   type Price_Type is private;

   function "+" (Left, Right : Price_Type) return Price_Type;
   function "-" (Left, Right : Price_Type) return Price_Type;

   function "<"  (Left, Right : Price_Type) return Boolean;
   function ">"  (Left, Right : Price_Type) return Boolean;
   function "<=" (Left, Right : Price_Type) return Boolean;
   function ">=" (Left, Right : Price_Type) return Boolean;

   function To_Price (Amount : Real) return Price_Type;
   function To_Real (Price : Price_Type) return Real;

   function Adjust_Price (Price    : Price_Type;
                          Factor   : Real)
                         return Price_Type;

   function Tax (Price   : Price_Type;
                 Tax     : Quantities.Quantity)
                 return Price_Type;

   function Max (X, Y : Price_Type) return Price_Type;
   function Min (X, Y : Price_Type) return Price_Type;

   function Total (Price  : Price_Type;
                   Quantity : Quantities.Quantity)
                  return Money_Type;

   function Price (Total    : Money_Type;
                   Quantity : Quantities.Quantity)
                   return Price_Type;

   function Get_Quantity
     (Total_Cash : Money_Type;
      Price      : Price_Type)
      return Quantities.Quantity;

   function Image (Item : Money_Type) return String;
   function Image (Item : Price_Type) return String;

   function Value (Image : String) return Money_Type;
   function Value (Image : String) return Price_Type;

   function Split (Amount  : Money_Type;
                   Portion : Real)
                  return Money_Type;

   function Zero return Price_Type;

private

   type Money_Type is new Real;
   type Price_Type is new Real range 0.0 .. Real'Last;

   pragma Import (Intrinsic, "+");
   pragma Import (Intrinsic, "-");
   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">=");
   pragma Import (Intrinsic, "abs");

end Concorde.Money;
