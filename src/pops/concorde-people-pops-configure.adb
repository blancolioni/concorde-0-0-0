with Tropos.Reader;

with Concorde.Random;

with Concorde.Options;
with Concorde.Paths;

with WL.Money;

with Concorde.People.Pops.Db;

package body Concorde.People.Pops.Configure is

   Colony_Configs : Tropos.Configuration;
   Colony_Configs_Loaded : Boolean := False;

   procedure Check_Colony_Configs;

   procedure Create_Colony
     (System        : in out Concorde.Systems.Root_Star_System_Type'Class;
      Colony_Config : Tropos.Configuration);

   --------------------------
   -- Check_Colony_Configs --
   --------------------------

   procedure Check_Colony_Configs is
   begin
      if not Colony_Configs_Loaded then
         declare
            Scenario : constant String := Concorde.Options.Scenario;
         begin
            Colony_Configs :=
              Tropos.Reader.Read_Config
                (Concorde.Paths.Config_File
                   ("scenarios/" & Scenario & "/colonies.txt"));
            Colony_Configs_Loaded := True;
         end;
      end if;
   end Check_Colony_Configs;

   -------------------
   -- Create_Colony --
   -------------------

   procedure Create_Colony
     (System        : in out Concorde.Systems.Root_Star_System_Type'Class;
      Colony_Config : Tropos.Configuration)
   is
      Pop_Config : constant Tropos.Configuration :=
                     Colony_Config.Child ("pops");
   begin
      for Config of Pop_Config loop
         declare
            procedure Create (Pop : in out Root_Pop_Type'Class);

            ------------
            -- Create --
            ------------

            procedure Create (Pop : in out Root_Pop_Type'Class) is
               Group : constant Groups.Pop_Group :=
                         Groups.Get (Config.Config_Name);
               Size  : constant Positive := Config.Value;
               Cash  : constant WL.Money.Money_Type :=
                         WL.Money.To_Money
                           (Real (Size * Group.Initial_Cash_Factor)
                            * (0.5 + Concorde.Random.Unit_Random));
            begin
               Pop.Size := Pop_Size (Size);
               Pop.Groups.Replace_Element (Group.Reference, 1.0);
               Pop.Set_Cash (Cash);
            end Create;

            Pop : constant Pop_Type :=
                    Concorde.People.Pops.Db.Create (Create'Access);
         begin
            System.Add_Pop (Pop);
         end;
      end loop;
   end Create_Colony;

   -------------------
   -- Create_Colony --
   -------------------

   procedure Create_Colony
     (System        : in out Concorde.Systems.Root_Star_System_Type'Class;
      Template_Name : String)
   is
   begin
      Check_Colony_Configs;

      Create_Colony (System, Colony_Configs.Child (Template_Name));

   end Create_Colony;

end Concorde.People.Pops.Configure;
