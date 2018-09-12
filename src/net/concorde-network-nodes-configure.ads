with Concorde.Network.Expressions;

package Concorde.Network.Nodes.Configure is

   procedure Add_Effect
     (From       : in out Root_Node_Type'Class;
      To         : String;
      Expression : Expressions.Expression_Type;
      Wait       : Duration);

end Concorde.Network.Nodes.Configure;
