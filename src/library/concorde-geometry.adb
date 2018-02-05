with Concorde.Elementary_Functions;

package body Concorde.Geometry is

   function Real_To_Radians (R : Real) return Radians;

   ---------
   -- "*" --
   ---------

   function "*" (Left : Radians; Right : Real) return Radians is
   begin
      return Real_To_Radians (Real (Left) * Right);
   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (Left : Real; Right : Radians) return Radians is
   begin
      return Real_To_Radians (Left * Real (Right));
   end "*";

   ---------
   -- "+" --
   ---------

   overriding function "+" (Left, Right : Radians) return Radians is
      X : constant Real := Real (Left) + Real (Right);
   begin
      return Real_To_Radians (X);
   end "+";

   ---------
   -- "-" --
   ---------

   overriding function "-" (Left, Right : Radians) return Radians is
      X : constant Real := Real (Left) - Real (Right);
   begin
      return Real_To_Radians (X);
   end "-";

   ---------
   -- "/" --
   ---------

   function "/" (Left : Radians; Right : Real) return Radians is
   begin
      return Real_To_Radians (Real (Left) / Right);
   end "/";

   ---------
   -- "<" --
   ---------

   overriding function "<" (Left, Right : Radians) return Boolean is
   begin
      return Real (abs Left) < Real (abs Right);
   end "<";

   -----------
   -- "abs" --
   -----------

   overriding function "abs" (R : Radians) return Radians is
      X : constant Real := Real (R);
   begin
      if X < Ada.Numerics.Pi then
         return R;
      else
         return Radians (2.0 * Ada.Numerics.Pi - X);
      end if;
   end "abs";

   ----------------------
   -- Angular_Distance --
   ----------------------

   function Angular_Distance
     (From    : Point_Type;
      Heading : Radians;
      To      : Point_Type)
      return Radians
   is
      B : constant Radians := Bearing (From, To);
      D : constant Radians := abs (Heading - B);
   begin
      return D;
   end Angular_Distance;

   ---------
   -- Arc --
   ---------

   function Arc (From, To : Radians) return Arc_Type is
   begin
      return (From, To);
   end Arc;

   ------------
   -- Arccos --
   ------------

   function Arccos (Value : Signed_Unit_Real) return Radians is
   begin
      return Radians (Concorde.Elementary_Functions.Arccos (Value));
   end Arccos;

   ------------
   -- Arcsin --
   ------------

   function Arcsin (Value : Signed_Unit_Real) return Radians is
   begin
      return Radians (Concorde.Elementary_Functions.Arcsin (Value));
   end Arcsin;

   ------------
   -- Arctan --
   ------------

   function Arctan (Value : Real) return Radians is
   begin
      return Radians (Concorde.Elementary_Functions.Arctan (Value));
   end Arctan;

   ---------------------
   -- Arctan_Relative --
   ---------------------

   function Arctan_Relative (Value : Real) return Signed_Unit_Real is
   begin
      return Concorde.Elementary_Functions.Arctan (Value)
        / Ada.Numerics.Pi;
   end Arctan_Relative;

   -------------
   -- Bearing --
   -------------

   function Bearing
     (From : Point_Type;
      To   : Point_Type)
      return Radians
   is
      use Concorde.Elementary_Functions;
      Result : constant Real := Arctan (To.Y - From.Y, To.X - From.X);
   begin
      if Result < 0.0 then
         return Radians (Result + Ada.Numerics.Pi * 2.0);
      else
         return Radians (Result);
      end if;
   end Bearing;

   --------------
   -- Contains --
   --------------

   function Contains
     (Arc   : Arc_Type;
      Angle : Radians)
      return Boolean
   is
   begin
      if Arc.To > Arc.From then
         return Angle >= Arc.From and then Angle < Arc.To;
      else
         return Angle >= Arc.From or else Angle < Arc.To;
      end if;
   end Contains;

   ---------
   -- Cos --
   ---------

   function Cos (Angle : Radians) return Signed_Unit_Real is
   begin
      return Concorde.Elementary_Functions.Cos (Real (Angle));
   end Cos;

   ------------------------
   -- Degrees_To_Radians --
   ------------------------

   function Degrees_To_Radians
     (Degrees : Real)
      return Radians
   is
      D : constant Real := Degrees * Ada.Numerics.Pi / 180.0;
   begin
      return Real_To_Radians (D);
   end Degrees_To_Radians;

   ------------------------
   -- Radians_To_Degrees --
   ------------------------

   function Radians_To_Degrees
     (Angle : Radians)
      return Real
   is
   begin
      return Real (Angle) * 180.0 / Ada.Numerics.Pi;
   end Radians_To_Degrees;

   ---------------------
   -- Real_To_Radians --
   ---------------------

   function Real_To_Radians (R : Real) return Radians is
      D : Real := R;
      Tau : constant := 2.0 * Ada.Numerics.Pi;
   begin
      while D < 0.0 loop
         D := D + Tau;
      end loop;

      while D >= Tau loop
         D := D - Tau;
      end loop;

      return Radians (D);
   end Real_To_Radians;

   ---------
   -- Sin --
   ---------

   function Sin (Angle : Radians) return Signed_Unit_Real is
   begin
      return Concorde.Elementary_Functions.Sin (Real (Angle));
   end Sin;

   ---------
   -- Tan --
   ---------

   function Tan (Angle : Radians) return Real is
   begin
      return Concorde.Elementary_Functions.Tan (Real (Angle));
   end Tan;

   ----------
   -- Turn --
   ----------

   function Turn
     (Start     : Radians;
      Turn_Size : Radians;
      Towards   : Radians)
      return Radians
   is
      X : constant Real := Real (Start);
      Y : constant Real := Real (Towards);
   begin
      if Y > X then
         if Y - X > Ada.Numerics.Pi then
            return Start - Turn_Size;
         else
            return Start + Turn_Size;
         end if;
      else
         if X - Y > Ada.Numerics.Pi then
            return Start + Turn_Size;
         else
            return Start - Turn_Size;
         end if;
      end if;
   end Turn;

end Concorde.Geometry;
