with Concorde.Graphs;

package Concorde.Systems.Graphs is
  new Concorde.Graphs
    (Index_Type   => Positive,
     Vertex_Type  => Star_System_Type,
     Cost_Type    => Non_Negative_Real,
     Default_Cost => 1.0,
     Index_Of     => Get_Index);
