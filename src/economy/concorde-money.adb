with Ada.Strings.Fixed;

package body Concorde.Money is

   --  Pound : constant Character :=
   --    Ada.Characters.Latin_1.Pound_Sign;
   Dollar : constant Character := '$';

   Currency : constant Character := Dollar;

   ------------
   -- Adjust --
   ------------

   function Adjust (Money    : Money_Type;
                    Factor   : Non_Negative_Real)
                    return Money_Type
   is
   begin
      return Money * Money_Type (Factor);
   end Adjust;

   ------------------
   -- Adjust_Price --
   ------------------

   function Adjust_Price (Price    : Price_Type;
                          Factor   : Non_Negative_Real)
                         return Price_Type
   is
   begin
      return Price_Type (Real (Price) * Factor);
   end Adjust_Price;

   ------------------
   -- Get_Quantity --
   ------------------

   function Get_Quantity
     (Total_Cash : Money_Type;
      Price      : Price_Type)
      return Quantities.Quantity
   is
   begin
      return Quantities.To_Quantity
        (Real (Total_Cash) / Real (Price));
   end Get_Quantity;

   -----------
   -- Image --
   -----------

   function Image (Item : Money_Type) return String is
   begin
      if Item < 0 then
         return "(" & Image (Price_Type (abs Item)) & ")";
      else
         return Image (Price_Type (Item));
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   overriding function Image (Item : Price_Type) return String is
      Image : constant String :=
        Ada.Strings.Fixed.Trim (Price_Type'Image ((Item + 5) / 10),
                                Ada.Strings.Left);
   begin
      if Image'Length = 1 then
         return Currency & "0.0" & Image;
      elsif Image'Length = 2 then
         return Currency & "0." & Image;
      else
         return Currency & Image (Image'First .. Image'Last - 2) & "." &
           Image (Image'Last - 1 .. Image'Last);
      end if;
   end Image;

   ---------
   -- Max --
   ---------

   function Max (X, Y : Money_Type) return Money_Type is
   begin
      return Money_Type'Max (X, Y);
   end Max;

   ---------
   -- Max --
   ---------

   overriding function Max (X, Y : Price_Type) return Price_Type is
   begin
      return Price_Type'Max (X, Y);
   end Max;

   ---------
   -- Min --
   ---------

   function Min (X, Y : Money_Type) return Money_Type is
   begin
      return Money_Type'Min (X, Y);
   end Min;

   ---------
   -- Min --
   ---------

   overriding function Min (X, Y : Price_Type) return Price_Type is
   begin
      return Price_Type'Min (X, Y);
   end Min;

   -----------
   -- Price --
   -----------

   function Price (Total    : Money_Type;
                   Quantity : Quantities.Quantity)
                   return Price_Type
   is
   begin
      return Price_Type (Real (Total) / Quantities.To_Real (Quantity));
   end Price;

   -----------
   -- Split --
   -----------

   function Split
     (Amount  : Money_Type;
      Portion : Unit_Real)
      return Money_Type
   is
   begin
      return Money_Type (Real (Amount) * Portion);
   end Split;

   ---------
   -- Tax --
   ---------

   function Tax (Money : Money_Type;
                 Tax   : Non_Negative_Real)
                 return Money_Type
   is
   begin
      if Money < 0 then
         return 0;
      else
         return Money_Type (Real (Money) * Tax);
      end if;
   end Tax;

   ---------
   -- Tax --
   ---------

   overriding function Tax (Price   : Price_Type;
                            Tax     : Non_Negative_Real)
                            return Price_Type
   is
   begin
      return Price_Type (Real (Price) * Tax);
   end Tax;

   --------------
   -- To_Money --
   --------------

   function To_Money (Amount : Real) return Money_Type is
   begin
      return Money_Type (Amount * 1000.0);
   end To_Money;

   -------------------
   -- To_Price_Type --
   -------------------

   function To_Price (Amount : Real) return Price_Type is
   begin
      return Price_Type (Amount * 1000.0);
   end To_Price;

   -------------
   -- To_Real --
   -------------

   function To_Real (Amount : Money_Type) return Real is
   begin
      return Real (Amount) / 1000.0;
   end To_Real;

   -------------
   -- To_Real --
   -------------

   overriding function To_Real (Price : Price_Type) return Real is
   begin
      return Real (Price) / 1000.0;
   end To_Real;

   -----------
   -- Total --
   -----------

   function Total (Price  : Price_Type;
                   Quantity : Quantities.Quantity)
                  return Money_Type
   is
      use Quantities;
   begin
      return Money_Type (Real (Price) * To_Real (Quantity));
   end Total;

   -----------
   -- Value --
   -----------

   function Value (Image : String) return Money_Type is
   begin
      return To_Money (Real'Value (Image));
   end Value;

   -----------
   -- Value --
   -----------

   overriding function Value (Image : String) return Price_Type is
   begin
      return To_Price (Real'Value (Image));
   end Value;

   -----------------
   -- Without_Tax --
   -----------------

   function Without_Tax
     (Money : Money_Type;
      Tax   : Non_Negative_Real)
      return Money_Type
   is
   begin
      if Money < 0 then
         return Money;
      else
         return Money_Type (Real (Money) / (1.0 + Tax));
      end if;
   end Without_Tax;

   -----------------
   -- Without_Tax --
   -----------------

   overriding function Without_Tax
     (Price   : Price_Type;
      Tax     : Non_Negative_Real)
      return Price_Type
   is
   begin
      return Price_Type (Without_Tax (Money_Type (Price), Tax));
   end Without_Tax;

   ----------
   -- Zero --
   ----------

   function Zero return Money_Type is
   begin
      return 0;
   end Zero;

   ----------
   -- Zero --
   ----------

   overriding function Zero return Price_Type is
   begin
      return 0;
   end Zero;

end Concorde.Money;
