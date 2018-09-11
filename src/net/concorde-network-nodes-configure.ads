with Concorde.Expressions;

package Concorde.Network.Nodes.Configure is

   procedure Add_Output
     (From       : in out Root_Node_Type'Class;
      To         : not null access Root_Node_Type'Class;
      Expression : Concorde.Expressions.Expression_Type;
      Inertia    : Duration);

   procedure Initial_Values
     (Network : Node_Map'Class;
      Get_Value : not null access
        function (Name : String) return Real);

end Concorde.Network.Nodes.Configure;
