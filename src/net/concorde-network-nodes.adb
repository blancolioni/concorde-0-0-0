with Concorde.Network.State;

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

   --------------
   -- Add_Node --
   --------------

   overriding procedure Add_Node
     (State : in out Node_State_Map;
      Node  : Node_State_Access)
   is
   begin
      State.Map.Insert (Node.Identifier, Node);
   end Add_Node;

   ------------------
   -- Create_State --
   ------------------

   function Create_State
     (Node          : not null access constant Root_Node_Type;
      Initial_Value : Real)
      return Node_State_Access
   is
   begin
      return State : constant Concorde.Network.Node_State_Access :=
        Concorde.Network.State.New_Internal_Node_State (Node)
      do
         State.Set_Initial_Value (Initial_Value);
      end return;
   end Create_State;

   -------------------------
   -- Evaluate_Constraint --
   -------------------------

   overriding function Evaluate_Constraint
     (From             : Node_State_Map;
      Class_Name       : String;
      Constraint_Name  : String;
      Constraint_Value : String)
      return Array_Of_Values
   is
      pragma Unreferenced (From, Class_Name, Constraint_Name,
                           Constraint_Value);
      Values : Array_Of_Values (1 .. 0);
   begin
      return Values;
   end Evaluate_Constraint;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise
     (Node : in out Root_Node_Type'Class;
      Id   : String)
   is
   begin
      Node.Id := new String'(Id);
   end Initialise;

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
