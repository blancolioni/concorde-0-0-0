with Tropos.Reader;

with Concorde.Configure;
with Concorde.Politics.Configure;

with Concorde.Network.Expressions.Parser;

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
                   (Path      =>
                      Concorde.Configure.Directory_Path
                        ("groups"),
                    Extension => "group");

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

         procedure Configure_Proportion (Pop_Config : Tropos.Configuration);

         --------------------------
         -- Configure_Proportion --
         --------------------------

         procedure Configure_Proportion (Pop_Config : Tropos.Configuration) is
            P : constant Float := Pop_Config.Get ("base", 0.0);
            W : constant String := Pop_Config.Get ("wealth-bias", "");
            L : constant Boolean := Pop_Config.Get ("left");
         begin
            Group.Default_Proportion := Unit_Real (P);
            if W /= "" then
               Group.Expression_Proportion :=
                 Concorde.Network.Expressions.Parser.Parse_String (W);
               Group.Wealth_Proportion := True;
            end if;
            Group.Left_Bias := L;
            if Pop_Config.Contains ("left") then
               Group.Political_Wing := True;
            end if;
         end Configure_Proportion;

      begin
         Group.Set_Local_Tag (Name);
         Concorde.Politics.Configure.Configure
           (Group.Default_Politics, Config.Child ("politics"));
         Group.Wealth_Group := Config.Get ("wealth-group");
         Configure_Proportion (Config.Child ("proportion"));
      end Create;

   begin
      Vector.Append (Db.Create (Create'Access));
   end Create_Pop_Group;

end Concorde.People.Groups.Configure;
