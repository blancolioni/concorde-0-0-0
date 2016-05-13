with Ada.Text_IO;

with Tropos.Reader;

with Memor.Element_Vectors;

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
with Concorde.Government.Create;

with Concorde.Facilities.Db;

with Concorde.People.Skills.Db;
with Concorde.Installations.Db;

with Concorde.Systems.Db;

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

      use Concorde.Quantities;

      package Skilled_Pop_Vectors is
        new Memor.Element_Vectors (Quantity, Zero);

      Skilled_Pop : Skilled_Pop_Vectors.Vector;

      Hub : constant Concorde.Installations.Installation_Type :=
              Concorde.Installations.Create.Create
                (Location =>
                           Concorde.Systems.Db.Reference (System),
                 Facility => Concorde.Facilities.Colony_Hub,
                 Cash     =>
                   Concorde.Money.To_Money
                     (Template.Get ("cash", 10_000.0)),
                 Owner    => System.Owner);

      Government : constant Concorde.Government.Government_Type :=
                     Concorde.Government.Create.Create_Government
                       (Governed          =>
                         Concorde.Systems.Db.Reference (System),
                        Cash              =>
                          Concorde.Money.To_Money
                            (Template.Get ("cash", 10_000.0)),
                        Owner             => System.Owner,
                        Headquarters      => Hub,
                        Basic_Living_Wage =>
                          Template.Get ("basic_living_wage", False));

      procedure Create_Pop_From_Config
        (Config : Tropos.Configuration);

      procedure Create_Pop_From_Skill
        (Reference : Memor.Database_Reference;
         Element   : Quantity);

      procedure Create_Pop
        (Group : Concorde.People.Groups.Pop_Group;
         Skill : Concorde.People.Skills.Pop_Skill;
         Size  : Concorde.People.Pops.Pop_Size);

      procedure Create_Installation
        (Facility : Concorde.Facilities.Facility_Type);

      procedure Add_Inputs
        (Installation : Concorde.Installations.Installation_Type);

      procedure Add_Population
        (Installation : Concorde.Installations.Installation_Type);

      procedure Create_Service_Facilities is null;

      ----------------
      -- Add_Inputs --
      ----------------

      procedure Add_Inputs
        (Installation : Concorde.Installations.Installation_Type)
      is
      begin
         for I in 1 .. Installation.Facility.Input_Count loop
            declare
               Need : constant Concorde.Commodities.Commodity_Type :=
                        Installation.Facility.Input_Commodity (I);
               Quant : constant Quantity :=
                         Installation.Facility.Input_Quantity (I)
                         * Installation.Facility.Capacity_Quantity
                         * To_Quantity (5.0);
               Value    : constant Concorde.Money.Money_Type :=
                            Concorde.Money.Total
                              (Need.Base_Price, Quant);

               procedure Add_Stock
                 (To : in out Installations.Root_Installation_Type'Class);

               ---------------
               -- Add_Stock --
               ---------------

               procedure Add_Stock
                 (To : in out Installations.Root_Installation_Type'Class)
               is
               begin
                  To.Add_Quantity (Need, Quant, Value);
               end Add_Stock;

            begin
               Concorde.Installations.Db.Update
                 (Hub.Reference, Add_Stock'Access);
            end;
         end loop;
      end Add_Inputs;

      --------------------
      -- Add_Population --
      --------------------

      procedure Add_Population
        (Installation : Concorde.Installations.Installation_Type)
      is
      begin
         for I in 1 .. Installation.Facility.Worker_Count loop
            declare
               Skill : constant Concorde.People.Skills.Pop_Skill :=
                         Installation.Facility.Worker_Skill (I);
               Quant : constant Quantity :=
                         Installation.Facility.Worker_Quantity (I);
               Current : constant Quantity  :=
                           Skilled_Pop.Element (Skill.Reference);
            begin
               Skilled_Pop.Replace_Element
                 (Skill.Reference, Current + Quant);
            end;
         end loop;
      end Add_Population;

      -------------------------
      -- Create_Installation --
      -------------------------

      procedure Create_Installation
        (Facility : Concorde.Facilities.Facility_Type)
      is
         Installation : constant Concorde.Installations.Installation_Type :=
                          Concorde.Installations.Create.Create
                            (Location =>
                                 Concorde.Systems.Db.Reference (System),
                             Facility => Facility,
                             Cash     => Concorde.Money.To_Money (1.0E5),
                             Owner    => System.Owner);
      begin
         System.Add_Installation (Installation);
         Add_Population (Installation);
         Add_Inputs (Installation);
      end Create_Installation;

      ----------------
      -- Create_Pop --
      ----------------

      procedure Create_Pop
        (Group : Concorde.People.Groups.Pop_Group;
         Skill : Concorde.People.Skills.Pop_Skill;
         Size  : Concorde.People.Pops.Pop_Size)
      is
         use Concorde.Commodities;
         Cash  : constant Non_Negative_Real :=
                   Real (Size) * Real (Group.Initial_Cash_Factor);
         Pop   : constant Concorde.People.Pops.Pop_Type :=
                   Concorde.People.Pops.Create.New_Pop
                     (Location => Concorde.Systems.Db.Reference (System),
                      Location_Index => 0,
                      Wealth_Group => Group,
                      Skill        => Skill,
                      Size         => Size,
                      Cash         => Concorde.Money.To_Money (Cash));
         Needs : constant Array_Of_Commodities :=
                   Concorde.Commodities.Get
                     (Consumer, Group.Preferred_Quality);
      begin
         System.Add_Pop (Pop);

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
                  Quantity : constant Concorde.Quantities.Quantity :=
                               Concorde.Quantities.To_Quantity
                                 (Real (Pop.Size) * 30.0);
                  Value    : constant Concorde.Money.Money_Type :=
                               Concorde.Money.Total
                                 (Need.Base_Price, Quantity);
               begin
                  Installation.Add_Quantity (Need, Quantity, Value);
               end Add_Hub_Stock;

            begin
               Concorde.Installations.Db.Update
                 (Hub.Reference, Add_Hub_Stock'Access);
            end;
         end loop;

      end Create_Pop;

      ----------------------------
      -- Create_Pop_From_Config --
      ----------------------------

      procedure Create_Pop_From_Config
        (Config : Tropos.Configuration)
      is
--           use Concorde.Commodities;
         use Concorde.People.Groups;
         use Concorde.People.Skills;
         Group : constant Pop_Group := Get (Config.Config_Name);
         Skill : constant Pop_Skill := Get (Config.Get ("skill", "unskilled"));
         Size  : constant Natural := Config.Get ("size");
      begin
         Create_Pop
           (Group, Skill,
            Concorde.People.Pops.Pop_Size (Size));
      end Create_Pop_From_Config;

      ---------------------------
      -- Create_Pop_From_Skill --
      ---------------------------

      procedure Create_Pop_From_Skill
        (Reference : Memor.Database_Reference;
         Element   : Quantity)
      is
         Skill : constant Concorde.People.Skills.Pop_Skill :=
                   Concorde.People.Skills.Db.Element (Reference);
      begin
         Create_Pop (Skill.Wealth_Group, Skill,
                     Concorde.People.Pops.Pop_Size
                       (Quantities.To_Real (Element)));
      end Create_Pop_From_Skill;

   begin

      System.Set_Government (Government);
      System.Add_Installation (Hub);
      Add_Population (Hub);

      for Pop_Config of Template.Child ("pops") loop
         Create_Pop_From_Config (Pop_Config);
      end loop;

      declare
         Install_Config : constant Tropos.Configuration :=
                            Template.Child ("installations");

         procedure Configure_Installation
           (Facility : Concorde.Facilities.Facility_Type);

         ----------------------------
         -- Configure_Installation --
         ----------------------------

         procedure Configure_Installation
           (Facility : Concorde.Facilities.Facility_Type)
         is
            Id : constant String := Facility.Identifier;
         begin
            if Install_Config.Contains (Id) then
               Ada.Text_IO.Put_Line
                 (Install_Config.Get (Id) & " x " & Id);
            end if;

            for I in 1 .. Install_Config.Get (Facility.Identifier, 0) loop
               Create_Installation (Facility);
            end loop;
         end Configure_Installation;

      begin
         for I in 1 .. Install_Config.Get ("resource_generator", 0) loop
            Create_Installation
              (Concorde.Facilities.Resource_Generator
                 (System.Resource));
         end loop;

         Concorde.Facilities.Db.Scan (Configure_Installation'Access);

      end;

      Create_Service_Facilities;

      Skilled_Pop.Iterate (Create_Pop_From_Skill'Access);

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
