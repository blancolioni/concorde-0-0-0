with Ada.Characters.Handling;
with Ada.Strings.Fixed;

with Tropos.Reader;

with Concorde.Configure;

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
                              (Concorde.Configure.File_Path
                                 ("facilities", "facilities"));
   begin
      for Config of Facilities_Config loop
         if Ada.Strings.Fixed.Index (Config.Config_Name, "_template") = 0 then
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
         if Template_Config.Config_Name /= "" then
            Facility.Resource_Name := new String'(Template_Config.Config_Name);
         end if;

         Facility.Class :=
           Facility_Class'Value
             (Value ("type", "bad class in " & Config.Config_Name));

         Facility.Set_Local_Tag (Config.Config_Name);
         Facility.Base_Service_Charge :=
           Concorde.Money.Value (Value ("service_charge", "0"));
         Facility.Power :=
           Concorde.Quantities.Value (Value ("power", "0"));
         Facility.Workforce :=
           Concorde.Quantities.Value (Value ("workforce", "0"));

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

         declare
            Worker_Config : Tropos.Configuration;
         begin
            if Config.Contains ("employees") then
               Worker_Config := Config.Child ("employees");
            elsif Template_Config.Contains ("employees") then
               Worker_Config := Template_Config.Child ("employees");
            end if;

            declare
               Worker_Array  : Array_Of_Workers
                 (1 .. Worker_Config.Child_Count);
               Count        : Natural := 0;
            begin
               for Cfg of Worker_Config loop
                  Count := Count + 1;
                  Worker_Array (Count) :=
                    Worker_Record'
                      (Group    =>
                         Concorde.People.Groups.Get (Cfg.Get ("poptype")),
                       Activity => Work,
                       Effect   =>
                         Process_Effect'Value (Cfg.Get ("effect")),
                       Proportion =>
                         Unit_Real (Float'(Cfg.Get ("amount"))));
               end loop;
               Facility.Workers := new Array_Of_Workers'(Worker_Array);
            end;
         end;

         declare
            Owner_Config : constant Tropos.Configuration :=
                             (if Config.Contains ("owner")
                              then Config.Child ("owner")
                              else Template_Config.Child ("owner"));
         begin
            Facility.Owner_Group :=
              Concorde.People.Groups.Get (Owner_Config.Get ("poptype"));
            Facility.Owner_Effect :=
              Process_Effect'Value (Owner_Config.Get ("effect"));
            Facility.Owner_Factor :=
              Real (Float'(Owner_Config.Get ("effect_multiplier", 0.0)));
            Facility.Owner_Activity := Manage;
         end;

         declare

            function Configure_Input_Array
              (Config : Tropos.Configuration)
               return Array_Of_Inputs;

            ---------------------------
            -- Configure_Input_Array --
            ---------------------------

            function Configure_Input_Array
              (Config : Tropos.Configuration)
               return Array_Of_Inputs
            is
               Input_Array  : Array_Of_Inputs (1 .. Config.Child_Count);
               Count        : Natural := 0;
            begin
               for Cfg of Config loop
                  Count := Count + 1;
                  declare
                     Input_Name : constant String :=
                                    Cfg.Config_Name;
                     New_Input  : Input_Record_Access;
                  begin
                     if Input_Name = "or" then
                        New_Input := new Input_Record'
                          (Class     => Choice,
                           Choices   =>
                              new Array_Of_Inputs'
                             (Configure_Input_Array (Cfg)));
                     else
                        if not Exists (Input_Name) then
                           raise Constraint_Error with
                             "while configuring facility " & Facility.Tag.all
                             & ": undefined input: " & Input_Name;
                        end if;
                        New_Input := new Input_Record'
                          (Class     => Simple,
                           Commodity => Get (Input_Name),
                           Quantity  => Cfg.Value);
                     end if;
                     Input_Array (Count) := New_Input;
                  end;
               end loop;
               return Input_Array;
            end Configure_Input_Array;

         begin
            if Config.Contains ("input_goods") then
               Facility.Inputs := new Array_Of_Inputs'
                 (Configure_Input_Array (Config.Child ("input_goods")));
            end if;
         end;

         if Config.Contains ("output_goods")
           or else Template_Config.Contains ("output_goods")
         then
            declare
               Output_Name : constant String :=
                               Value ("output_goods", "");
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
                  Facility.Output := Get (Value ("output_goods", ""));
                  Facility.Output_Value := Real'Value (Value ("value", "0.0"));
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
