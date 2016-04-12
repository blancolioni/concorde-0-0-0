with Ada.Strings.Fixed;

with Concorde.Random;

package body Concorde.Quantities is

   function Significant_Digits_Image (Item : Quantity;
                                      Sig  : Positive)
                                     return String;

   ------------
   -- Around --
   ------------

   function Around (X : Quantity) return Quantity is
   begin
      return Quantity
        (Real (X - X / 10.0)
         + (Concorde.Random.Unit_Random
           * Real (X / 5.0)));
   end Around;

   -----------
   -- Image --
   -----------

   function Image (Item : Quantity) return String is

      Factors    : constant array (1 .. 3) of Quantity :=
        (1.0E9, 1.0E6, 1.0E3);
      Extensions : constant String := "GMK";
   begin
      for I in Factors'Range loop
         if Item > Factors (I) then
            return Significant_Digits_Image (Item / Factors (I), 3) &
              (1 => Extensions (I));
         end if;
      end loop;

      return Significant_Digits_Image (Item, 3);
   end Image;

   ---------
   -- Max --
   ---------

   function Max (Left, Right : Quantity) return Quantity is
   begin
      return Quantity'Max (Left, Right);
   end Max;

   ---------
   -- Min --
   ---------

   function Min (Left, Right : Quantity) return Quantity is
   begin
      return Quantity'Min (Left, Right);
   end Min;

   ------------------------------
   -- Significant_Digits_Image --
   ------------------------------

   function Significant_Digits_Image (Item : Quantity;
                                      Sig  : Positive)
                                     return String
   is
      Result    : String (1 .. Sig);
      Point     : Natural := 0;
      Acc       : Quantity := Item;
      Boundary  : constant Quantity := 10.0**Sig;
   begin
      if Item < 1.0 / Boundary then
         return "0.00";
      end if;

      if abs Item >= Boundary then
         return Ada.Strings.Fixed.Trim (Integer'Image (Integer (Item)),
                                        Ada.Strings.Left);
      else
         while abs Acc * 10.0 < Boundary loop
            Acc := Acc * 10.0;
            Point := Point + 1;
         end loop;

         Result :=
           Ada.Strings.Fixed.Trim (Integer'Image (Integer (Acc - 0.5)),
                                   Ada.Strings.Left);
         if Point < Sig then
            if Point = 0 then
               return Result;
            else
               return Result (1 .. Result'Last - Point) & "." &
                 Result (Result'Last - Point + 1 .. Result'Last);
            end if;
         else
            declare
               Zeroes : constant String (1 .. Point - Sig) :=
                 (others => '0');
            begin
               return "0." & Zeroes & Result;
            end;
         end if;
      end if;
   end Significant_Digits_Image;

   ----------------
   -- To_Natural --
   ----------------

   function To_Natural (Value : Quantity) return Natural is
   begin
      return Natural (Value);
   end To_Natural;

   -----------------
   -- To_Quantity --
   -----------------

   function To_Quantity (Value : Real) return Quantity is
   begin
      return Quantity (Value);
   end To_Quantity;

   --------------
   -- To_Real --
   --------------

   function To_Real (Value : Quantity) return Real is
   begin
      return Real (Value);
   end To_Real;

   ----------
   -- Unit --
   ----------

   function Unit return Quantity is
   begin
      return 1.0;
   end Unit;

   ----------
   -- Zero --
   ----------

   function Zero return Quantity is
   begin
      return 0.0;
   end Zero;

end Concorde.Quantities;