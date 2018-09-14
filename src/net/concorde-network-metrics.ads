private with Ada.Containers.Doubly_Linked_Lists;

with Concorde.Network.Expressions;
with Concorde.Network.Nodes;

package Concorde.Network.Metrics is

   type Root_Metric_Type is
     abstract new Concorde.Network.Nodes.Root_Node_Type with private;
   type Metric_Type is access all Root_Metric_Type'Class;

   type Metric_Operator is (Add, Multiply);

   procedure Add_Calculation
     (To         : in out Root_Metric_Type'Class;
      Operator   : Metric_Operator;
      Expression : Concorde.Network.Expressions.Expression_Type);

   function New_Money_Metric
     (Id : String)
      return Metric_Type;

   function New_Rating_Metric
     (Id : String)
      return Metric_Type;

private

   type Calculation_Record is
      record
         Operator   : Metric_Operator;
         Expression : Concorde.Network.Expressions.Expression_Type;
      end record;

   package List_Of_Calculations is
     new Ada.Containers.Doubly_Linked_Lists (Calculation_Record);

   type Root_Metric_Type is
     abstract new Concorde.Network.Nodes.Root_Node_Type with
      record
         Calculation : List_Of_Calculations.List;
      end record;

end Concorde.Network.Metrics;
