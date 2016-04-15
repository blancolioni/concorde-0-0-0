with Tropos.Reader;

with Concorde.Options;
with Concorde.Paths;

with Concorde.Money;
with Concorde.Quantities;

with Concorde.Commodities;
with Concorde.Empires;
with Concorde.Facilities;
with Concorde.Installations;

with Concorde.People.Groups;
with Concorde.People.Skills;

with Concorde.Installations.Create;
with Concorde.People.Pops.Create;

with Concorde.Installations.Db;

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

      Hub : constant Concorde.Installations.Installation_Type :=
              Concorde.Installations.Create.Create
                (Facility => Concorde.Facilities.Colony_Hub,
                 Cash     =>
                   Concorde.Money.To_Money
                     (Template.Get ("cash", 10_000.0)),
                 Owner    => System.Owner);

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
                            (Facility => Facility,
                             Cash     => Concorde.Money.To_Money (1.0E3),
                             Owner    => System.Owner);
      begin
         System.Add_Installation (Installation);
      end Create_Installation;

      ----------------
      -- Create_Pop --
      ----------------

      procedure Create_Pop
        (Config : Tropos.Configuration)
      is
         use Concorde.Commodities;
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
         Needs : constant Array_Of_Commodities :=
                   Concorde.Commodities.Get
                     (Consumer, Group.Preferred_Quality);

      begin
         System.Add_Pop (Pop);
         System.Add_Installation (Hub);

         for Need of Needs loop
            declare
               procedure Add_Hub_Stock
                 (Installation : in out
                    Concorde.Installations.Root_Installation_Type'Class);

               -------------------
               -- Add_Hub_Stock --
               -------------------

               procedure Add_Hub_Stock
                 (Installation : in out
                    Concorde.Installations.Root_Installation_Type'Class)
               is
               begin
                  Installation.Add_Quantity
                    (Need,
                     Concorde.Quantities.To_Quantity (Real (Pop.Size) * 30.0));
               end Add_Hub_Stock;

            begin
               Concorde.Installations.Db.Update
                 (Hub.Reference, Add_Hub_Stock'Access);
            end;
         end loop;

      end Create_Pop;

   begin

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
