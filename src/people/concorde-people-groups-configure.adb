with Tropos.Reader;

with Concorde.Money;

with Concorde.Configure;
with Concorde.Politics.Configure;
with Concorde.Commodities.Configure;

with Concorde.Network.Expressions;
with Concorde.Network.Expressions.Parser;
with Concorde.Network.Metrics;

package body Concorde.People.Groups.Configure is

   procedure Create_Pop_Group
     (Config    : Tropos.Configuration);

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
   end Configure_Pop_Groups;

   ----------------------
   -- Create_Pop_Group --
   ----------------------

   procedure Create_Pop_Group
     (Config    : Tropos.Configuration)
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

         Wealth : constant String := Config.Get ("wealth", "missing");

      begin

         Group.Set_Local_Tag (Name);
         Concorde.Politics.Configure.Configure
           (Group.Default_Politics, Config.Child ("politics"));

         Group.Wealth :=
           (if Wealth = "rich" then Rich
            elsif Wealth = "middle-class" then Middle_Class
            elsif Wealth = "poor" then Poor
            else raise Constraint_Error
              with "bad wealth: " & Wealth & " in configuration for pop group "
            & Name);

         Group.State_Employee := Config.Get ("state-employee");
         Group.Wealth_Group := Config.Get ("wealth-group");
         Configure_Proportion (Config.Child ("proportion"));
         if Config.Get ("has-commodity") then
            Group.Pop_Group_Commodity :=
              Concorde.Commodities.Configure.New_Pop_Group
                (Group.Identifier,
                 Base_Price => Concorde.Money.To_Price (1.0));
         end if;

      end Create;

      Group : constant Pop_Group := Db.Create (Create'Access);

   begin

      declare
         procedure Create_Metrics (Group : in out Root_Pop_Group'Class);

         --------------------
         -- Create_Metrics --
         --------------------

         procedure Create_Metrics (Group : in out Root_Pop_Group'Class) is
         begin
            Group.Income_Node :=
              Concorde.Network.Nodes.Node_Type
                (Concorde.Network.Metrics.New_Money_Metric
                   (Group.Identifier & "-income"));
            Group.Frequency_Node :=
              Concorde.Network.Nodes.Node_Type
                (Concorde.Network.Metrics.New_Money_Metric
                   (Group.Identifier & "-frequency"));
            Group.Happiness_Node :=
              Concorde.Network.Nodes.Node_Type
                (Concorde.Network.Metrics.New_Rating_Metric
                   (Group.Identifier));
            Concorde.Network.Nodes.Add_Node (Group.Happiness_Node);
            Concorde.Network.Nodes.Add_Node (Group.Frequency_Node);
            Concorde.Network.Nodes.Add_Node (Group.Income_Node);

         end Create_Metrics;

      begin
         Db.Update (Group.Reference, Create_Metrics'Access);
      end;

      Vector.Append (Group);
   end Create_Pop_Group;

end Concorde.People.Groups.Configure;
