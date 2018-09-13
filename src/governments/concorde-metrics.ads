private with Ada.Containers.Vectors;
private with Memor.Database;

with Concorde.Objects;

with Concorde.Network.Expressions;
with Concorde.Network.Nodes;

package Concorde.Metrics is

   type Root_Metric_Type is
     new Concorde.Objects.Root_Localised_Object_Type with private;

   type Metric_Type is access constant Root_Metric_Type'Class;

   procedure Scan_Metrics
     (Process : not null access
        procedure (Metric : Metric_Type));

   function Metric_Node
     (Metric : Root_Metric_Type'Class)
      return Concorde.Network.Nodes.Node_Type;

private

   type Root_Metric_Type is
     new Concorde.Objects.Root_Localised_Object_Type with
      record
         Active       : Boolean := False;
         Node         : Concorde.Network.Nodes.Node_Type;
         Actual_Value : Concorde.Network.Expressions.Expression_Type;
      end record;

   overriding function Object_Database
     (Item : Root_Metric_Type)
      return Memor.Memor_Database;

   package Db is
     new Memor.Database
       ("Metric", Root_Metric_Type, Metric_Type);

   overriding function Object_Database
     (Item : Root_Metric_Type)
      return Memor.Memor_Database
   is (Db.Get_Database);

   package Metric_Vectors is
     new Ada.Containers.Vectors (Positive, Metric_Type);

   Vector : Metric_Vectors.Vector;

end Concorde.Metrics;
