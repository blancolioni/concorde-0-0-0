with Ada.Characters.Handling;

with Tropos.Reader;

with Concorde.Paths;

with Concorde.Facilities.Db;

package body Concorde.Facilities.Configure is

   procedure Configure_Facility
     (Config : Tropos.Configuration);

   ---------------------------
   -- Configure_Facilities --
   ---------------------------

   procedure Configure_Facilities is
      Facilities_Config : constant Tropos.Configuration :=
                             Tropos.Reader.Read_Config
                               (Concorde.Paths.Config_File
                                  ("facilities/facilities.txt"));
   begin
      for Config of Facilities_Config loop
         Configure_Facility (Config);
      end loop;
   end Configure_Facilities;

   -------------------------
   -- Configure_Facility --
   -------------------------

   procedure Configure_Facility
     (Config : Tropos.Configuration)
   is

      procedure Create (Facility : in out Root_Facility_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Facility : in out Root_Facility_Type'Class) is
         use Concorde.Commodities;
         Template : constant Facility_Type :=
                      (if Config.Contains ("template")
                       then Get (Config.Get ("template"))
                       else null);
      begin
         Facility.Tag := new String'(Config.Config_Name);
         Facility.Class :=
           (if Template /= null
            then Template.Class
            else Facility_Class'Value (Config.Get ("class")));
         Facility.Set_Name (Config.Get ("name", Config.Config_Name));
         Facility.Base_Service_Charge :=
           Concorde.Money.Value (Config.Get ("service_charge", "0"));
         Facility.Power :=
           (if Template /= null
            then Template.Power
            else Concorde.Quantities.Value (Config.Get ("power", "0")));
         Facility.Capacity :=
           (if Template /= null
            then Template.Capacity
            else Facility_Capacity'Value (Config.Get ("capacity", "0")));

         if Template /= null then
            Facility.Flags := Template.Flags;
         else
            Facility.Flags := (others => False);
         end if;

         for Flag in Facility.Flags'Range loop
            Facility.Flags (Flag) :=
              Facility.Flags (Flag)
              or else
                Config.Get
                  (Ada.Characters.Handling.To_Lower
                     (Facility_Flag'Image (Flag)));
         end loop;

         if Template /= null then
            Facility.Commodity_Flags := Template.Commodity_Flags;
         else
            Facility.Commodity_Flags := (others => False);
         end if;

         for Flag in Facility.Commodity_Flags'Range loop
            Facility.Commodity_Flags (Flag) :=
              Facility.Commodity_Flags (Flag)
              or else Config.Get
                (Ada.Characters.Handling.To_Lower
                   (Commodity_Flag'Image (Flag)));
         end loop;
         Facility.Quality :=
           Commodity_Quality'Val (Config.Get ("quality", 2) - 1);

         if Config.Contains ("inputs") then
            declare
               Input_Config : constant Tropos.Configuration :=
                                Config.Child ("inputs");
               Input_Array  : Array_Of_Inputs (1 .. Input_Config.Child_Count);
               Count        : Natural := 0;
            begin
               for Cfg of Input_Config loop
                  Count := Count + 1;
                  Input_Array (Count).Commodity := Get (Cfg.Config_Name);
                  Input_Array (Count).Quantity :=
                    Concorde.Quantities.Value (Cfg.Value);
               end loop;
               Facility.Inputs := new Array_Of_Inputs'(Input_Array);
            end;
         end if;

         if Config.Contains ("output") then
            Facility.Output := Get (Config.Get ("output"));
         end if;

      end Create;
   begin
      Concorde.Facilities.Db.Create (Create'Access);
   end Configure_Facility;

end Concorde.Facilities.Configure;
