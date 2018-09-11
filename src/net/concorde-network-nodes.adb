package body Concorde.Network.Nodes is

   Map : Node_Network;

   --------------
   -- Add_Node --
   --------------

   procedure Add_Node
     (Node : Node_Type)
   is
   begin
      Map.Map.Insert (Node.Identifier, Node);
   end Add_Node;

   ----------
   -- Node --
   ----------

   overriding function Node
     (State : Node_State_Map;
      Name  : String)
      return Node_State_Access
   is
   begin
      return State.Map.Element (Name);
   end Node;

   ----------
   -- Node --
   ----------

   function Node
     (Name    : String)
      return Node_Type
   is
   begin
      return Map.Map.Element (Name);
   end Node;

   -----------------
   -- Scan_Inputs --
   -----------------

   procedure Scan_Inputs
     (Node    : Root_Node_Type'Class;
      Process : not null access
        procedure (Input_Node : Node_Type;
                   Input_Inertia : Duration;
                   Expression : Concorde.Network.Expressions.Expression_Type))
   is
   begin
      for Input of Node.Inputs loop
         Process (Input.Source, Input.Inertia, Input.Expression);
      end loop;
   end Scan_Inputs;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (State : in out Node_State_Map)
   is
   begin
      for St of State.Map loop
         St.Calculate_New_Value (State);
      end loop;
      for St of State.Map loop
         St.Set_New_Value;
      end loop;
   end Update;

end Concorde.Network.Nodes;
