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

   -------------------
   -- Scan_Policies --
   -------------------

   procedure Scan_Policies
     (Process : not null access
        procedure (Policy : Policy_Type))
   is
   begin
      Db.Scan (Process);
   end Scan_Policies;

end Concorde.Policies;
