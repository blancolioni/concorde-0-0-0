with Ada.Characters.Handling;

with Tropos.Reader;

with Concorde.Paths;

with Concorde.Commodities.Db;

package body Concorde.Commodities.Configure is

   procedure Configure_Commodity
     (Config : Tropos.Configuration);

   ---------------------------
   -- Configure_Commodities --
   ---------------------------

   procedure Configure_Commodities is
      Commodities_Config : constant Tropos.Configuration :=
                             Tropos.Reader.Read_Config
                               (Concorde.Paths.Config_File
                                  ("commodities/commodities.txt"));
   begin
      for Config of Commodities_Config loop
         Configure_Commodity (Config);
      end loop;
   end Configure_Commodities;

   -------------------------
   -- Configure_Commodity --
   -------------------------

   procedure Configure_Commodity
     (Config : Tropos.Configuration)
   is
      procedure Create (Commodity : in out Root_Commodity_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Commodity : in out Root_Commodity_Type'Class) is
      begin
         Commodity.Tag := new String'(Config.Config_Name);
         Commodity.Class := Commodity_Class'Value (Config.Get ("class"));
         Commodity.Set_Name (Config.Get ("name", Config.Config_Name));
         Commodity.Mass := Non_Negative_Real (Float'(Config.Get ("mass")));
         Commodity.Base_Price :=
           Concorde.Money.Value (Config.Get ("base_price", "0"));
         for Flag in Commodity.Flags'Range loop
            Commodity.Flags (Flag) :=
              Config.Get
                (Ada.Characters.Handling.To_Lower
                   (Commodity_Flag'Image (Flag)));
         end loop;
         Commodity.Quality :=
           Commodity_Quality'Val (Config.Get ("quality", 2) - 1);
      end Create;
   begin
      Concorde.Commodities.Db.Create (Create'Access);
   end Configure_Commodity;

end Concorde.Commodities.Configure;
