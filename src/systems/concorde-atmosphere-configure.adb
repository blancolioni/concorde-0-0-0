with Tropos.Reader;

with Concorde.Paths;

with Concorde.Atmosphere.Db;

package body Concorde.Atmosphere.Configure is

   procedure Configure_Gas
     (Config : Tropos.Configuration);

   --------------------------
   -- Configure_Atmosphere --
   --------------------------

   procedure Configure_Atmosphere is
      Config : constant Tropos.Configuration :=
                 Tropos.Reader.Read_Config
                   (Concorde.Paths.Config_File
                      ("star-systems/atmosphere.txt"));
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
      begin
         Gas.Set_Local_Tag (Config.Config_Name);
         Gas.Formula :=
           new String'(Config.Get ("formula", Config.Config_Name));
         Gas.Molecular_Weight := Config.Get ("weight");
         Gas.Melting_Point := Config.Get ("mp");
         Gas.Boiling_Point := Config.Get ("bp");
         Gas.Density := Config.Get ("density");
         Gas.Abundance_E := Config.Get ("abunde");
         Gas.Abundance_S := Config.Get ("abunds");
         Gas.Reactivity := Config.Get ("react");
         Gas.Max_IPP := Config.Get ("max_ipp");
      end Create;

   begin
      Db.Create (Create'Access);
   end Configure_Gas;

end Concorde.Atmosphere.Configure;
