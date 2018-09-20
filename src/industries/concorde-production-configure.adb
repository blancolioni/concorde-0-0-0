with Ada.Text_IO;

with Tropos.Reader;

with Concorde.Configure;
with Concorde.Real_Images;

package body Concorde.Production.Configure is

   procedure Create_Production
     (Config    : Tropos.Configuration);

   procedure Configure_Production is
      Config : constant Tropos.Configuration :=
                 Tropos.Reader.Read_Config
                   (Path      =>
                      Concorde.Configure.Directory_Path
                        ("production"),
                    Extension => "production");

   begin
      for Production_Config of Config loop
         Create_Production (Production_Config);
      end loop;
   end Configure_Production;

   -----------------------
   -- Create_Production --
   -----------------------

   procedure Create_Production
     (Config    : Tropos.Configuration)
   is
      Name  : constant String := Config.Config_Name;

      procedure Create (Production : in out Root_Production_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Production : in out Root_Production_Type'Class) is

      begin
         Ada.Text_IO.Put_Line ("production: " & Name);
         Production.Set_Local_Tag (Name);
         for Employee_Config of Config.Child ("employees") loop
            Ada.Text_IO.Put_Line
              ("   "
               & Concorde.Commodities.Get (Employee_Config.Config_Name).Name
               & " "
               & Concorde.Real_Images.Approximate_Image
                 (Real (Float'(Employee_Config.Value))));
            Production.Inputs.Append
              (Production_Commodity'
                 (Commodity         =>
                      Concorde.Commodities.Get (Employee_Config.Config_Name),
                  Relative_Quantity =>
                    Real (Float'(Employee_Config.Value)),
                  Consumption       => 0.0));
         end loop;
         for Input_Config of Config.Child ("inputs") loop
            Production.Inputs.Append
              (Production_Commodity'
                 (Commodity         =>
                      Concorde.Commodities.Get (Input_Config.Config_Name),
                  Relative_Quantity =>
                    Real (Float'(Input_Config.Value)),
                  Consumption       => 1.0));
         end loop;

      end Create;

      Production : constant Production_Type := Db.Create (Create'Access);

   begin
      Vector.Append (Production);
   end Create_Production;

end Concorde.Production.Configure;
