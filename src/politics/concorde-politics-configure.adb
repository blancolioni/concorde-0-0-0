package body Concorde.Politics.Configure is

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Rec    : in out Political_Record'Class;
      Config : Tropos.Configuration)
   is
   begin
      for Axis_Config of Config loop
         declare
            Axis : Political_Axis;

            function Value (Field   : String;
                            Default : Unit_Real)
                            return Unit_Real
            is (Unit_Real (Float'(Axis_Config.Get (Field, Float (Default)))));

            function Position return Unit_Real
            is (Value ("position", 0.0));

            function Strength return Unit_Real
            is (Value ("strength", 0.0));

         begin
            Axis := Political_Axis'Value (Axis_Config.Config_Name);
            Rec.Axis (Axis).Position := Position;
            Rec.Axis (Axis).Strength := Strength;
         end;
      end loop;
   end Configure;

end Concorde.Politics.Configure;
