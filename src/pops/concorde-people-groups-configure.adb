with Tropos.Reader;

with Concorde.Paths;

package body Concorde.People.Groups.Configure is

   procedure Configure_Pop_Group
     (Config : Tropos.Configuration);

   procedure Create_Pop_Group
     (Config : Tropos.Configuration);

   -------------------------
   -- Configure_Pop_Group --
   -------------------------

   procedure Configure_Pop_Group
     (Config : Tropos.Configuration)
   is
   begin
      null;
   end Configure_Pop_Group;

   --------------------------
   -- Configure_Pop_Groups --
   --------------------------

   procedure Configure_Pop_Groups is
      Config : constant Tropos.Configuration :=
                 Tropos.Reader.Read_Config
                   (Concorde.Paths.Config_File
                      ("pops/pop_group.txt"));
   begin
      for Group_Config of Config loop
         Create_Pop_Group (Group_Config);
      end loop;
      for Group_Config of Config loop
         Configure_Pop_Group (Group_Config);
      end loop;
   end Configure_Pop_Groups;

   ----------------------
   -- Create_Pop_Group --
   ----------------------

   procedure Create_Pop_Group
     (Config : Tropos.Configuration)
   is
      Name  : constant String := Config.Config_Name;

      procedure Create (Group : in out Root_Pop_Group'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Group : in out Root_Pop_Group'Class) is
      begin
         Group.Set_Local_Tag (Name);
         Group.Initial_Cash_Factor := Config.Get ("initial_cash", 0);
         Group.Preferred_Quality :=
           Concorde.Commodities.Commodity_Quality'Val
             (Config.Get ("quality", 2) - 1);
         if Config.Contains ("needs") then
            for Need_Config of Config.Child ("needs") loop
               if Need_Config.Config_Name = "energy" then
                  Group.Energy_Needs :=
                    Non_Negative_Real
                      (Float'(Need_Config.Value));
               else
                  declare
                     use Concorde.Commodities;
                     Commodity : constant Commodity_Type :=
                                   Get (Need_Config.Config_Name);
                     Need      : constant Float := Need_Config.Value;
                  begin
                     Group.Needs.Append
                       (Need_Record'
                          (Commodity => Commodity,
                           Need      => Non_Negative_Real (Need)));
                  end;
               end if;
            end loop;
         end if;
      end Create;

   begin
      Db.Create (Create'Access);
   end Create_Pop_Group;

end Concorde.People.Groups.Configure;
