with Tropos.Reader;

with Concorde.Configure;

with Concorde.Network.Metrics;

package body Concorde.Metrics.Configure is

   procedure Configure_Metric
     (Config : Tropos.Configuration);

   ----------------------
   -- Configure_Metric --
   ----------------------

   procedure Configure_Metric
     (Config : Tropos.Configuration)
   is
      procedure Create (Metric : in out Root_Metric_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Metric : in out Root_Metric_Type'Class) is
         Data_Type_Name : constant String :=
                            Config.Get ("type", "rating");
      begin
         Metric.Set_Local_Tag (Config.Config_Name);

         if Data_Type_Name = "rating" then
            Metric.Node :=
              Concorde.Network.Metrics.New_Rating_Metric (Config.Config_Name);
         elsif Data_Type_Name = "money" then
            Metric.Node :=
              Concorde.Network.Metrics.New_Money_Metric (Config.Config_Name);
         else
            raise Constraint_Error with
              "unrecognised metric type: " & Data_Type_Name;
         end if;
      end Create;

      Metric : constant Metric_Type :=
                 Db.Create (Create'Access);
   begin
      Concorde.Network.Nodes.Add_Node (Metric.Node);
      Vector.Append (Metric);
   end Configure_Metric;

   -----------------------
   -- Configure_Metrics --
   -----------------------

   procedure Configure_Metrics is
   begin
      Tropos.Reader.Read_Config
        (Path      => Concorde.Configure.Directory_Path ("metrics"),
         Extension => "metric",
         Configure => Configure_Metric'Access);
   end Configure_Metrics;

end Concorde.Metrics.Configure;
