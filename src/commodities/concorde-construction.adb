with Concorde.Quantities;

package body Concorde.Construction is

   ------------
   -- Create --
   ------------

   procedure Create
     (Rec       : in out Constructed_Record'Class;
      Construct : Concorde.Commodities.Stock_Interface'Class;
      Maintain  : Concorde.Commodities.Stock_Interface'Class)
   is
   begin
      Rec.Construct.Create_Stock
        (Concorde.Quantities.To_Quantity (100_000.0), True);
      Rec.Construct.Add (Construct);
      Rec.Maintain.Create_Stock
        (Concorde.Quantities.To_Quantity (1_000.0), True);
      Rec.Maintain.Add (Maintain);
   end Create;

end Concorde.Construction;
