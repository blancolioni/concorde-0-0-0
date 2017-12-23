with Tropos.Reader;

with Lui.Colours.Config;

with Concorde.Configure;

package body Concorde.Features.Configure is

   procedure Configure_Feature
     (Config : Tropos.Configuration);

   -----------------------
   -- Configure_Feature --
   -----------------------

   procedure Configure_Feature
     (Config : Tropos.Configuration)
   is

      procedure Create (Feature : in out Root_Feature_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Feature : in out Root_Feature_Type'Class) is
      begin
         Feature.Set_Local_Tag (Config.Config_Name);
         Feature.Is_Ice := Config.Get ("ice", False);
         Feature.Is_Desert := Config.Config_Name = "desert";
         Feature.Colour :=
           Lui.Colours.Config.Configure_Colour
             (Config, "colour");
      end Create;

   begin
      Db.Create (Create'Access);
   end Configure_Feature;

   -----------------------
   -- Configure_Feature --
   -----------------------

   procedure Configure_Features is
      Config : constant Tropos.Configuration :=
                 Tropos.Reader.Read_Config
                   (Concorde.Configure.File_Path
                      ("star-systems", "features"));
   begin
      for Feature_Config of Config loop
         Configure_Feature (Feature_Config);
      end loop;
   end Configure_Features;

end Concorde.Features.Configure;
