private with Ada.Numerics;

package Concorde.Geometry is

   type Radians is private;

   function Degrees_To_Radians
     (Degrees : Real)
      return Radians;

   function Bearing
     (From : Point_Type;
      To   : Point_Type)
      return Radians;

   function Angular_Distance
     (From    : Point_Type;
      Heading : Radians;
      To      : Point_Type)
      return Radians;

   function Turn
     (Start     : Radians;
      Turn_Size : Radians;
      Towards   : Radians)
      return Radians;

   function Cos (Angle : Radians) return Signed_Unit_Real;
   function Sin (Angle : Radians) return Signed_Unit_Real;
   function Tan (Angle : Radians) return Real;

   function Arccos (Value : Signed_Unit_Real) return Radians;
   function Arcsin (Value : Signed_Unit_Real) return Radians;
   function Arctan (Value : Real) return Radians;

   function Arctan_Relative (Value : Real) return Signed_Unit_Real;

   function "+" (Left, Right : Radians) return Radians;
   function "-" (Left, Right : Radians) return Radians;

   function "<" (Left, Right : Radians) return Boolean;

   function ">" (Left, Right : Radians) return Boolean
   is (Right < Left);

   function "<=" (Left, Right : Radians) return Boolean
   is (not (Left > Right));

   function ">=" (Left, Right : Radians) return Boolean
   is (not (Left < Right));

   type Arc_Type is private;

   function Arc (From, To : Radians) return Arc_Type;
   --  create arc anticlockwise From .. To

   function Degree_Arc (From, To : Real) return Arc_Type
   is (Arc (Degrees_To_Radians (From), Degrees_To_Radians (To)));

   function Contains (Arc   : Arc_Type;
                      Angle : Radians)
                      return Boolean;

private

   type Radians is new Real range 0.0 .. 2.0 * Ada.Numerics.Pi;

   function "abs" (R : Radians) return Radians;

   type Arc_Type is
      record
         From, To : Radians;
      end record;

end Concorde.Geometry;
