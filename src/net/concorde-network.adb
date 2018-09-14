package body Concorde.Network is

   -----------------------
   -- Run_Network_State --
   -----------------------

   procedure Run_Network_State
     (State : in out Network_State_Interface'Class)
   is
      procedure Send_Node_Effects
        (Node : Node_State_Access);

      procedure Update_Node_State
        (Node : Node_State_Access);

      -----------------------
      -- Send_Node_Effects --
      -----------------------

      procedure Send_Node_Effects
        (Node : Node_State_Access)
      is
      begin
         Node.Send_Effects (State);
      end Send_Node_Effects;

      -----------------------
      -- Update_Node_State --
      -----------------------

      procedure Update_Node_State
        (Node : Node_State_Access)
      is
      begin
         Node.Set_New_Value (State);
      end Update_Node_State;

   begin
      State.Scan_State_Nodes (Send_Node_Effects'Access);
      State.Scan_State_Nodes (Update_Node_State'Access);
   end Run_Network_State;

   -------------------------
   -- To_Expression_Value --
   -------------------------

   function To_Expression_Value (X : Real) return Expression_Value is
   begin
      return Expression_Value'
        (Real_Value   => X,
         Object_Value => null);
   end To_Expression_Value;

   -------------------------
   -- To_Expression_Value --
   -------------------------

   function To_Expression_Value
     (X : not null access constant Expression_Object_Interface'Class)
      return Expression_Value
   is
   begin
      return Expression_Value'
        (Real_Value   => 0.0,
         Object_Value => Expression_Object (X));
   end To_Expression_Value;

end Concorde.Network;
