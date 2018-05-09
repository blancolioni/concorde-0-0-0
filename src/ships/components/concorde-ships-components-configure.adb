with Ada.Text_IO;

with Tropos.Reader;

with Concorde.Configure;

package body Concorde.Ships.Components.Configure is

   procedure Configure_Component
     (Config : Tropos.Configuration);

   -------------------------
   -- Configure_Component --
   -------------------------

   procedure Configure_Component
     (Config : Tropos.Configuration)
   is

      function Get (Real_Name : String) return Real
      is (Real (Float'(Config.Get (Real_Name, 0.0))));

      procedure Create (Component : in out Root_Component_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Component : in out Root_Component_Type'Class) is
      begin
         Component.Set_Local_Tag (Config.Config_Name);
         declare
            Shape : constant String := Config.Get ("shape", "");
         begin
            if Shape /= "" then
               Component.Shape := Component_Shape'Value (Shape);
            else
               Component.Shape := Rectangular_Prism;
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "in configuration for " & Config.Config_Name
                  & ": no shape");
            end if;
         exception
            when others =>
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "in configuration for " & Config.Config_Name
                  & ": unknown shape: " & Shape);
         end;

         Component.Mass := Non_Negative_Real (Float'(Config.Get ("mass")));
         Component.Cargo := Config.Contains ("cargo");
         if Component.Cargo then
            Component.Payload_Volume :=
              Non_Negative_Real
                (Float'(Config.Child ("cargo").Get ("volume")));
         end if;
         Component.Tank := Config.Contains ("fuel");
         if Component.Tank then
            Component.Payload_Volume :=
              Non_Negative_Real
                (Float'(Config.Child ("fuel").Get ("volume")));
            Component.Cryogenic :=
              Config.Child ("fuel").Get ("cryo");
         end if;

         Component.Heat := Get ("heat");

         Component.Throttled := Config.Get ("throttled");
         Component.Thruster := Config.Contains ("thrust_maximum");
         Component.Thrust_Maximum := Get ("thrust_maximum");
         Component.Thrust_Minimum := Get ("thrust_minimum");
         Component.Exhaust_Velocity := Get ("ve");

         Component.Fuel_Is_Propellant :=
           Config.Get ("fuel", "") = "propellant";

         for Propellant_Config of Config.Child ("propellant") loop
            declare
               Rec : constant Component_Fuel_Record :=
                       Component_Fuel_Record'
                         (Fuel  =>
                            Concorde.Commodities.Get
                              (Propellant_Config.Config_Name),
                          Ratio =>
                            Non_Negative_Real
                              (Float'(Propellant_Config.Get ("ratio"))));
            begin
               Component.Propellant.Append (Rec);
            end;
         end loop;

      exception
         when others =>
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "error while configuring component: "
               & Config.Config_Name);
            raise;
      end Create;

   begin
      Db.Create (Create'Access);
   end Configure_Component;

   --------------------------
   -- Configure_Components --
   --------------------------

   procedure Configure_Components is
      Base_Path : constant String :=
                    Concorde.Configure.Directory_Path ("ships/components");
   begin
      Tropos.Reader.Read_Config
        (Path      => Base_Path,
         Extension => "txt",
         Configure => Configure_Component'Access);
   end Configure_Components;

end Concorde.Ships.Components.Configure;
