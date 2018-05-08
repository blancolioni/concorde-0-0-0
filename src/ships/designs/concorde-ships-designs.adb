package body Concorde.Ships.Designs is

   --------------------
   -- Cargo_Capacity --
   --------------------

   function Cargo_Capacity
     (Design : Root_Design_Type'Class)
      return WL.Quantities.Quantity_Type
   is
      Volume : Non_Negative_Real := 0.0;
   begin
      for Installed of Design.Installed_Modules loop
         Volume := Volume + Installed.Module.Component.Cargo_Payload_Volume;
      end loop;
      return WL.Quantities.To_Quantity (Float (Volume));
   end Cargo_Capacity;

end Concorde.Ships.Designs;
