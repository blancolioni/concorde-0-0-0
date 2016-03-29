with Tropos.Reader;

with Concorde.Paths;

with Concorde.Components.Manager;

package body Concorde.Components.Configure is

   type Component_Access is access all Root_Component_Type'Class;

   procedure Configure_Component
     (Config : Tropos.Configuration);

   -------------------------
   -- Configure_Component --
   -------------------------

   procedure Configure_Component
     (Config : Tropos.Configuration)
   is
      Class : constant Component_Class :=
                Component_Class'Value
                  (Config.Get ("class"));
      Shape : constant Component_Shape :=
                Component_Shape'Value
                  (Config.Get ("shape", "cube"));

      function Get (Name : String) return Real
      is (Config.Get (Name, 0.0));

      Component : constant Component_Access :=
                    new Root_Component_Type'
                      (Concorde.Objects.Root_Named_Object_Type with
                       Class                   => Class,
                       Shape                   => Shape,
                       Density                 => Get ("density"),
                       Crew                    => Config.Get ("crew", 1),
                       Nominal_Max_Output      => Get ("nominal_max_output"),
                       Nominal_Charge          => Get ("nominal_charge"),
                       Nominal_Power_Draw      => Get ("nominal_power"),
                       Nominal_Heat_Production => Get ("nominal_heat"),
                       Output_Size_Power       => Get ("output_size_power"),
                       Input_Size_Power        => Get ("input_size_power"),
                       Charge_Size_Power       => Get ("charge_size_power"),
                       Heat_Size_Power         => Get ("heat_size_power"),
                       Throttle_Step           => Get ("throttle_step"),
                       Size_Throttle_Factor    => Get ("size_throttle_factor"),
                       Energy_Coefficient      => Get ("energy_coefficient"),
                       Energy_From_Fuel        =>
                         Config.Get ("energy_from_fuel"),
                       Explosion_Chance        => Get ("explode"),
                       Layout                  => (1, 1, 1));

   begin
      Component.Set_Name (Config.Get ("name", Config.Config_Name));

      if Config.Contains ("layout") then
         declare
            Layout_Config : constant Tropos.Configuration :=
                              Config.Child ("layout");
         begin
            Component.Layout := (Layout_Config.Get (1),
                                 Layout_Config.Get (2),
                                 Layout_Config.Get (3));
         end;

      end if;
      Concorde.Components.Manager.Register
        (Config.Config_Name,
         Component);
   end Configure_Component;

   --------------------------
   -- Configure_Components --
   --------------------------

   procedure Configure_Components is
   begin
      Tropos.Reader.Read_Config
        (Concorde.Paths.Config_File ("ships/components"),
         "txt",
         Configure_Component'Access);
   end Configure_Components;

end Concorde.Components.Configure;
