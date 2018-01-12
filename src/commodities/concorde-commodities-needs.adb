package body Concorde.Commodities.Needs is

   --------------
   -- Add_Need --
   --------------

   procedure Add_Need
     (Need      : in out Commodity_Needs;
      Commodity : Commodity_Type;
      Quantity  : WL.Quantities.Quantity_Type;
      Price     : WL.Money.Price_Type)
   is
      use WL.Money;
   begin
      Need.Vector.Append
        (Need_Record'
           (Commodity => Commodity,
            Quantity  => Quantity,
            Price     => Price));
      Need.Total_Cost := Need.Total_Cost + Total (Price, Quantity);
      Need.Scale := To_Float (Need.Budget) / To_Float (Need.Total_Cost);
      Need.Scale := Float'Min (Need.Scale, 1.0);
   end Add_Need;

   ----------------
   -- Scan_Needs --
   ----------------

   procedure Scan_Needs
     (Need    : Commodity_Needs;
      Process : not null access
        procedure (Commodity : Commodity_Type;
                   Quantity  : WL.Quantities.Quantity_Type;
                   Price     : WL.Money.Price_Type))
   is
   begin
      for Item of Need.Vector loop
         Process (Item.Commodity,
                  WL.Quantities.Scale (Item.Quantity, Need.Scale),
                  Item.Price);
      end loop;
   end Scan_Needs;

   ----------------
   -- Set_Budget --
   ----------------

   procedure Set_Budget
     (Need   : in out Commodity_Needs;
      Budget : WL.Money.Money_Type)
   is
      use WL.Money;
   begin
      Need.Budget := Budget;
      if Need.Total_Cost > Zero then
         Need.Scale := To_Float (Need.Budget) / To_Float (Need.Total_Cost);
         Need.Scale := Float'Min (Need.Scale, 1.0);
      end if;
   end Set_Budget;

end Concorde.Commodities.Needs;
