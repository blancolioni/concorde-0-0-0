with Ada.Text_IO;

with Tropos.Reader;

with Concorde.Configure;
with Concorde.Real_Images;

package body Concorde.Production.Configure is

   procedure Create_Production
     (Config    : Tropos.Configuration;
      Resource  : Concorde.Commodities.Commodity_Type := null);

   --------------------------
   -- Configure_Production --
   --------------------------

   procedure Configure_Production is
      Config : constant Tropos.Configuration :=
                 Tropos.Reader.Read_Config
                   (Path      =>
                      Concorde.Configure.Directory_Path
                        ("production"),
                    Extension => "production");

   begin
      for Production_Config of Config loop
         if Production_Config.Contains ("across") then
            for Across_Config of Production_Config.Child ("across") loop
               Create_Production
                 (Production_Config,
                  Concorde.Commodities.Get (Across_Config.Config_Name));
            end loop;
         else
            Create_Production (Production_Config);
         end if;
      end loop;
   end Configure_Production;

   -----------------------
   -- Create_Production --
   -----------------------

   procedure Create_Production
     (Config    : Tropos.Configuration;
      Resource  : Concorde.Commodities.Commodity_Type := null)
   is
      use type Concorde.Commodities.Commodity_Type;

      Name  : constant String :=
                (if Resource = null
                 then Config.Config_Name
                 else Resource.Identifier & "-" & Config.Config_Name);

      function Configure_Input
        (Config              : Tropos.Configuration;
         Default_Consumption : Unit_Real := 1.0)
         return Production_Commodity;

      procedure Create (Production : in out Root_Production_Type'Class);

      ---------------------
      -- Configure_Input --
      ---------------------

      function Configure_Input
        (Config              : Tropos.Configuration;
         Default_Consumption : Unit_Real := 1.0)
         return Production_Commodity
      is
         Quantity : constant Float :=
                      (if Config.Contains ("quantity")
                       then Config.Get ("quantity")
                       else Config.Value);
         Consumption : constant Float :=
                         Config.Get ("consumption",
                                     Float (Default_Consumption));
      begin
         return Production_Commodity'
           (Commodity         => Concorde.Commodities.Get (Config.Config_Name),
            Relative_Quantity => Non_Negative_Real (Quantity),
            Consumption       => Unit_Real (Consumption));
      end Configure_Input;

      ------------
      -- Create --
      ------------

      procedure Create (Production : in out Root_Production_Type'Class) is

      begin
         Ada.Text_IO.Put_Line ("production: " & Name);
         Production.Set_Local_Tag (Name);

         declare
            Owner_Tag : constant String :=
                          Config.Get ("owner", "");
         begin
            if Owner_Tag = "" then
               raise Constraint_Error with
                 "in production: " & Name & ": owner tag not set";
            elsif not Concorde.People.Groups.Exists (Owner_Tag) then
               raise Constraint_Error with
                 "in production: " & Name & ": no such pop group: "
                 & Owner_Tag;
            end if;

            Production.Owner :=
              Concorde.People.Groups.Get (Owner_Tag);
         end;

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
                  Consumption       => 1.0));
         end loop;

         for Input_Config of Config.Child ("inputs") loop
            Production.Inputs.Append (Configure_Input (Input_Config));
         end loop;

         for Output_Config of Config.Child ("outputs") loop
            if Output_Config.Config_Name = "current" then
               Production.Outputs.Append
                 (Production_Commodity'
                    (Commodity         => Resource,
                     Relative_Quantity =>
                       Real (Float'(Output_Config.Value)),
                     Consumption       => 0.0));
            else
               Production.Outputs.Append
                 (Production_Commodity'
                    (Commodity         =>
                         Concorde.Commodities.Get (Output_Config.Config_Name),
                     Relative_Quantity =>
                       Real (Float'(Output_Config.Value)),
                     Consumption       => 0.0));
            end if;
         end loop;
      end Create;

      Production : constant Production_Type := Db.Create (Create'Access);

   begin
      Vector.Append (Production);
   end Create_Production;

end Concorde.Production.Configure;
