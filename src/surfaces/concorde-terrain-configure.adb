with Tropos.Reader;

with Lui.Colors.Config;

with Concorde.Configure;

package body Concorde.Terrain.Configure is

   procedure Configure_Terrain
     (Config : Tropos.Configuration);

   -----------------------
   -- Configure_Terrain --
   -----------------------

   procedure Configure_Terrain is
      Config : constant Tropos.Configuration :=
                 Tropos.Reader.Read_Config
                   (Concorde.Configure.File_Path
                      ("star-systems", "terrain"));
   begin
      for Terrain_Config of Config loop
         Configure_Terrain (Terrain_Config);
      end loop;
   end Configure_Terrain;

   -----------------------
   -- Configure_Terrain --
   -----------------------

   procedure Configure_Terrain
     (Config : Tropos.Configuration)
   is

      procedure Create (Terrain : in out Root_Terrain_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Terrain : in out Root_Terrain_Type'Class) is
         Ch_Text : constant String := Config.Get ("char", "!");
      begin
         Terrain.Set_Local_Tag (Config.Config_Name);
         Terrain.Is_Water := Config.Get ("water", False);
         Terrain.Text_Character := Ch_Text (Ch_Text'First);
         Terrain.Color :=
           Lui.Colors.Config.Configure_Color
             (Config, "color");
      end Create;

   begin
      Db.Create (Create'Access);
   end Configure_Terrain;

end Concorde.Terrain.Configure;
