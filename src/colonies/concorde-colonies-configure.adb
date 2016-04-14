with Tropos.Reader;

with Concorde.Options;
with Concorde.Paths;

with Concorde.Money;

with Concorde.Empires;
with Concorde.Facilities;
with Concorde.Installations;

with Concorde.People.Groups;
with Concorde.People.Skills;

with Concorde.Installations.Create;
with Concorde.People.Pops.Create;

package body Concorde.Colonies.Configure is

   Template_Config : Tropos.Configuration;
   Got_Config      : Boolean := False;

   procedure Read_Config;

   procedure Create_Colony_From_Template
     (System   : in out Concorde.Systems.Root_Star_System_Type'Class;
      Template : Tropos.Configuration);

   ---------------------------------
   -- Create_Colony_From_Template --
   ---------------------------------

   procedure Create_Colony_From_Template
     (System        : in out Concorde.Systems.Root_Star_System_Type'Class;
      Template_Name : String)
   is
   begin
      if not Got_Config then
         Read_Config;
      end if;

      Create_Colony_From_Template
        (System, Template_Config.Child (Template_Name));

   end Create_Colony_From_Template;

   ---------------------------------
   -- Create_Colony_From_Template --
   ---------------------------------

   procedure Create_Colony_From_Template
     (System   : in out Concorde.Systems.Root_Star_System_Type'Class;
      Template : Tropos.Configuration)
   is

      procedure Create_Pop
        (Config : Tropos.Configuration);

      procedure Create_Installation
        (Facility : Concorde.Facilities.Facility_Type);

      procedure Create_Service_Facilities is null;

      -------------------------
      -- Create_Installation --
      -------------------------

      procedure Create_Installation
        (Facility : Concorde.Facilities.Facility_Type)
      is
         Installation : constant Concorde.Installations.Installation_Type :=
                          Concorde.Installations.Create.Create
                            (Facility, System.Owner);
      begin
         System.Add_Installation (Installation);
      end Create_Installation;

      ----------------
      -- Create_Pop --
      ----------------

      procedure Create_Pop
        (Config : Tropos.Configuration)
      is
         use Concorde.People.Groups;
         use Concorde.People.Skills;
         use Concorde.People.Pops;
         Group : constant Pop_Group := Get (Config.Config_Name);
         Size  : constant Natural := Config.Get ("size");
         Cash  : constant Non_Negative_Real :=
                   Real (Size) * Real (Group.Initial_Cash_Factor);
         Pop : constant Concorde.People.Pops.Pop_Type :=
                   Concorde.People.Pops.Create.New_Pop
                     (Wealth_Group => Group,
                      Skill        => Get (Config.Get ("skill", "unskilled")),
                      Size         => Pop_Size (Size),
                      Cash         => Concorde.Money.To_Money (Cash));
      begin
         System.Add_Pop (Pop);
      end Create_Pop;

   begin

      declare
         Hubs : constant Concorde.Facilities.Array_Of_Facilities :=
                  Concorde.Facilities.Get_By_Class
                    (Concorde.Facilities.Colony_Hub);
      begin
         Create_Installation (Hubs (Hubs'First));
      end;

      for Pop_Config of Template.Child ("pops") loop
         Create_Pop (Pop_Config);
      end loop;

      for I in 1 .. Template.Get ("resource_generator") loop
         Create_Installation
           (Concorde.Facilities.Resource_Generator
              (System.Resource));
      end loop;

      Create_Service_Facilities;

   end Create_Colony_From_Template;

   -----------------
   -- Read_Config --
   -----------------

   procedure Read_Config is
   begin
      Template_Config :=
        Tropos.Reader.Read_Config
          (Concorde.Paths.Config_File
             ("scenarios/"
              & Concorde.Options.Scenario
              & "/colonies.txt"));
      Got_Config := True;
   end Read_Config;

end Concorde.Colonies.Configure;
