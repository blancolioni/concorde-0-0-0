package body Concorde.Ships.Modules is

   ----------------------
   -- Thrust_Component --
   ----------------------

   function Thrust_Component
     (Module : Root_Module_Type'Class;
      Vector : Xi.Matrices.Vector_3)
      return Non_Negative_Real
   is
   begin
      return Module.Component.Thrust_Component
        (Module.Orientation, Vector);
   end Thrust_Component;

end Concorde.Ships.Modules;
