package body Concorde.Metrics is

   -----------------
   -- Metric_Node --
   -----------------

   function Metric_Node
     (Metric : Root_Metric_Type'Class)
      return Concorde.Network.Nodes.Node_Type
   is
   begin
      return Metric.Node;
   end Metric_Node;

   -------------------
   -- Scan_Metrics --
   -------------------

   procedure Scan_Metrics
     (Process : not null access
        procedure (Metric : Metric_Type))
   is
   begin
      Db.Scan (Process);
   end Scan_Metrics;

end Concorde.Metrics;
