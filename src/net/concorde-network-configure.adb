package body Concorde.Network.Configure is

   --------------
   -- New_Node --
   --------------

   procedure New_Node
     (State : in out Concorde.Network.Nodes.Node_Map'Class;
      Node  : Concorde.Network.Nodes.Root_Node_Type'Class)
   is
   begin
      State.Add_Node (Node);
   end New_Node;

end Concorde.Network.Configure;
