with Xi.Float_Arrays;

package body Concorde.Ships.Components is

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
            Thrust_Vector : constant Vector_3 :=
                              Orientation * (0.0, 0.0, 1.0);
         begin
            return Vector * Thrust_Vector;
         end;
      end if;
   end Thrust_Component;

end Concorde.Ships.Components;
