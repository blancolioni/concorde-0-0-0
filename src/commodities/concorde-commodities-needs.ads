private with Ada.Containers.Vectors;

package Concorde.Commodities.Needs is

   type Commodity_Needs is private;

   procedure Add_Need
     (Need      : in out Commodity_Needs;
      Commodity : Commodity_Type;
      Quantity  : WL.Quantities.Quantity_Type;
      Price     : WL.Money.Price_Type);

   procedure Set_Budget
     (Need   : in out Commodity_Needs;
      Budget : WL.Money.Money_Type);

   function Total_Cost (Need : Commodity_Needs) return WL.Money.Money_Type;

   procedure Scan_Needs
     (Need    : Commodity_Needs;
      Process : not null access
        procedure (Commodity : Commodity_Type;
                   Quantity  : WL.Quantities.Quantity_Type;
                   Price     : WL.Money.Price_Type));

private

   type Need_Record is
      record
         Commodity : Concorde.Commodities.Commodity_Type;
         Quantity  : WL.Quantities.Quantity_Type;
         Price     : WL.Money.Price_Type;
      end record;

   package Need_Vectors is
     new Ada.Containers.Vectors (Positive, Need_Record);

   type Commodity_Needs is
      record
         Vector     : Need_Vectors.Vector;
         Total_Cost : WL.Money.Money_Type := WL.Money.Zero;
         Budget     : WL.Money.Money_Type := WL.Money.Zero;
         Scale      : Float               := 1.0;
      end record;

   function Total_Cost (Need : Commodity_Needs) return WL.Money.Money_Type
   is (WL.Money.Min
       (WL.Money.Adjust (Need.Total_Cost, Float'Min (Need.Scale, 1.0)),
        Need.Budget));

end Concorde.Commodities.Needs;
