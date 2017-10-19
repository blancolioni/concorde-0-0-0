with Ada.Characters.Handling;

with Tropos.Reader;

with Concorde.Paths;

with Concorde.Commodities.Configure;

package body Concorde.Facilities.Configure is

   procedure Configure_Facility
     (Config          : Tropos.Configuration;
      Template_Config : Tropos.Configuration);

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
         if not Config.Get ("template-class") then
            declare
               Template_Config : Tropos.Configuration;
            begin
               if Config.Contains ("template") then
                  Template_Config :=
                    Facilities_Config.Child (Config.Get ("template", ""));
               end if;
               Configure_Facility (Config, Template_Config);
            end;
         end if;
      end loop;
   end Configure_Facilities;

   -------------------------
   -- Configure_Facility --
   -------------------------

   procedure Configure_Facility
     (Config          : Tropos.Configuration;
      Template_Config : Tropos.Configuration)
   is

      procedure Create (Facility : in out Root_Facility_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Facility : in out Root_Facility_Type'Class) is
         use Concorde.Commodities;
--           Template : constant Facility_Type :=
--                        (if Config.Contains ("template")
--                         then Get (Config.Get ("template"))
--                         else null);
         function Value (Tag : String; Default : String) return String
         is (Config.Get (Tag, Template_Config.Get (Tag, Default)));

      begin
         Facility.Tag := new String'(Config.Config_Name);
         Facility.Class :=
           Facility_Class'Value
             (Value ("class", "bad class in " & Config.Config_Name));
         Facility.Set_Local_Tag (Config.Config_Name);
         Facility.Base_Service_Charge :=
           WL.Money.Value (Config.Get ("service_charge", "0"));
         Facility.Power :=
           WL.Quantities.Value (Value ("power", "0"));

         declare
            Turnaround_Seconds : Float := 3600.0;
         begin
            if Config.Contains ("hours") then
               Turnaround_Seconds := Config.Get ("hours") * 3600.0;
            elsif Config.Contains ("days") then
               Turnaround_Seconds := Config.Get ("days") * 86_400.0;
            end if;
            Facility.Turnaround := Duration (Turnaround_Seconds);
         end;

         Facility.Capacity :=
           Facility_Capacity'Value (Value ("capacity", "0"));

         Facility.Flags := (others => False);

         for Flag in Facility.Flags'Range loop
            Facility.Flags (Flag) :=
              Facility.Flags (Flag)
              or else
                Config.Get
                  (Ada.Characters.Handling.To_Lower
                     (Facility_Flag'Image (Flag)))
              or else
                Template_Config.Get
                  (Ada.Characters.Handling.To_Lower
                     (Facility_Flag'Image (Flag)));
         end loop;

         Facility.Commodity_Flags := (others => False);

         for Flag in Facility.Commodity_Flags'Range loop
            Facility.Commodity_Flags (Flag) :=
              Facility.Commodity_Flags (Flag)
              or else Config.Get
                (Ada.Characters.Handling.To_Lower
                   (Commodity_Flag'Image (Flag)))
              or else Template_Config.Get
                (Ada.Characters.Handling.To_Lower
                   (Commodity_Flag'Image (Flag)));

         end loop;

         Facility.Quality :=
           Commodity_Quality'Val (Config.Get ("quality", 2) - 1);

         declare
            Worker_Config : Tropos.Configuration;
         begin
            if Config.Contains ("workers") then
               Worker_Config := Config.Child ("workers");
            elsif Template_Config.Contains ("workers") then
               Worker_Config := Template_Config.Child ("workers");
            end if;

            declare
               Worker_Array  : Array_Of_Workers
                 (1 .. Worker_Config.Child_Count);
               Count        : Natural := 0;
            begin
               for Cfg of Worker_Config loop
                  Count := Count + 1;
                  Worker_Array (Count).Skill :=
                    Concorde.People.Skills.Get (Cfg.Config_Name);
                  Worker_Array (Count).Quantity :=
                    WL.Quantities.Value (Cfg.Value);
               end loop;
               Facility.Workers := new Array_Of_Workers'(Worker_Array);
            end;
         end;

         declare
            Input_Config : Tropos.Configuration;
         begin
            if Config.Contains ("inputs") then
               Input_Config := Config.Child ("inputs");
            elsif Config.Contains ("inputs") then
               Input_Config := Config.Child ("inputs");
            end if;

            declare
               Input_Array  : Array_Of_Inputs (1 .. Input_Config.Child_Count);
               Count        : Natural := 0;
            begin
               for Cfg of Input_Config loop
                  Count := Count + 1;
                  declare
                     Input_Name : constant String :=
                                    Cfg.Config_Name;
                  begin
                     if not Exists (Input_Name) then
                        raise Constraint_Error with
                          "while configuring facility " & Facility.Tag.all
                          & ": undefined input: " & Input_Name;
                     end if;
                     Input_Array (Count).Commodity := Get (Input_Name);
                     Input_Array (Count).Quantity :=
                       WL.Quantities.Value (Cfg.Value);
                  end;
               end loop;
               Facility.Inputs := new Array_Of_Inputs'(Input_Array);
            end;
         end;

         if Config.Contains ("output")
           or else Template_Config.Contains ("output")
         then
            declare
               Output_Name : constant String :=
                               Value ("output", "");
            begin
               if Output_Name = "" then
                  raise Constraint_Error with
                    "while configuring facility " & Facility.Tag.all
                    & ": no output commodity found";
               elsif not Exists (Output_Name) then
                  raise Constraint_Error with
                    "while configuring facility " & Facility.Tag.all
                    & ": undefined output: " & Output_Name;
               else
                  Facility.Output := Get (Value ("output", ""));
               end if;
            end;
         end if;

      end Create;

      Facility : constant Facility_Type :=
                   Concorde.Facilities.Db.Create (Create'Access);
   begin
      if Facility.Class = Service_Facility then
         Concorde.Commodities.Configure.Create_From_Service
           (Facility);
      end if;
   end Configure_Facility;

end Concorde.Facilities.Configure;
