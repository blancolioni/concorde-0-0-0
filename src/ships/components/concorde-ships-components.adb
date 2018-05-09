with Xi.Float_Arrays;

package body Concorde.Ships.Components is

   ---------------------
   -- Scan_Propellant --
   ---------------------

   procedure Scan_Propellant
     (Component : Root_Component_Type'Class;
      Process   : not null access
        procedure (Propellant : Concorde.Commodities.Commodity_Type;
                   Ratio      : Non_Negative_Real))
   is
   begin
      for Propellant of Component.Propellant loop
         Process (Propellant.Fuel, Propellant.Ratio);
      end loop;
   end Scan_Propellant;

   ----------------------
   -- Thrust_Component --
   ----------------------

   function Thrust_Component
     (Component   : Root_Component_Type'Class;
      Orientation : Xi.Matrices.Matrix_4;
      Vector      : Xi.Matrices.Vector_3)
      return Non_Negative_Real
   is
   begin
      if not Component.Thruster then
         return 0.0;
      else
         declare
            use Xi.Matrices, Xi.Float_Arrays;
            Thrust_Vector : constant Vector_4 :=
                              Orientation * (0.0, 0.0, 1.0, 1.0);
         begin
            return Vector * Thrust_Vector (1 .. 3) / Thrust_Vector (4);
         end;
      end if;
   end Thrust_Component;

end Concorde.Ships.Components;
