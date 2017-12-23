with Tropos.Reader;

with Concorde.Configure;

package body Concorde.Atmosphere.Configure is

   procedure Configure_Gas
     (Config : Tropos.Configuration);

   --------------------------
   -- Configure_Atmosphere --
   --------------------------

   procedure Configure_Atmosphere is
      Config : constant Tropos.Configuration :=
                 Tropos.Reader.Read_Config
                   (Concorde.Configure.File_Path
                      ("star-systems", "atmosphere"));
   begin
      for Gas_Config of Config loop
         Configure_Gas (Gas_Config);
      end loop;
   end Configure_Atmosphere;

   -------------------
   -- Configure_Gas --
   -------------------

   procedure Configure_Gas
     (Config : Tropos.Configuration)
   is

      procedure Create (Gas : in out Root_Gas_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Gas : in out Root_Gas_Type'Class) is

         function Get (Name : String) return Real
         is (Real (Float'(Config.Get (Name))));

      begin
         Gas.Set_Local_Tag (Config.Config_Name);
         Gas.Formula :=
           new String'(Config.Get ("formula", Config.Config_Name));
         Gas.Molecular_Weight := Get ("weight");
         Gas.Melting_Point := Get ("mp");
         Gas.Boiling_Point := Get ("bp");
         Gas.Density := Get ("density");
         Gas.Abundance_E := Get ("abunde");
         Gas.Abundance_S := Get ("abunds");
         Gas.Reactivity := Get ("react");
         Gas.Max_IPP := Get ("max_ipp");
      end Create;

   begin
      Db.Create (Create'Access);
   end Configure_Gas;

end Concorde.Atmosphere.Configure;
