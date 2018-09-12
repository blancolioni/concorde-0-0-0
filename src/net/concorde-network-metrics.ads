with Concorde.Network.Nodes;

package Concorde.Network.Metrics is

   type Root_Metric_Type is
     abstract new Concorde.Network.Nodes.Root_Node_Type with private;
   type Metric_Type is access all Root_Metric_Type'Class;

   function New_Money_Metric
     (Id : String)
      return Concorde.Network.Nodes.Node_Type;

   function New_Rating_Metric
     (Id : String)
      return Concorde.Network.Nodes.Node_Type;

private

   type Root_Metric_Type is
     abstract new Concorde.Network.Nodes.Root_Node_Type with
      record
         null;
      end record;

end Concorde.Network.Metrics;
