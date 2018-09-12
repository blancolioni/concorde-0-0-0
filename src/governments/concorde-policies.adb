package body Concorde.Policies is

   -----------------
   -- Policy_Node --
   -----------------

   function Policy_Node
     (Policy : Root_Policy_Type'Class)
      return Concorde.Network.Nodes.Node_Type
   is
   begin
      return Policy.Node;
   end Policy_Node;

end Concorde.Policies;
