with Concorde.Network.State;

package body Concorde.Network.Nodes is

   Map : Node_Network;

   ---------------
   -- Add_Field --
   ---------------

   procedure Add_Field
     (Node       : in out Root_Node_Type'Class;
      Name       : String;
      Definition : Concorde.Network.Expressions.Expression_Type)
   is
   begin
      pragma Assert (not Node.Fields.Contains (Name));
      Node.Fields.Insert (Name, Definition);
      pragma Assert (Node.Fields.Contains (Name));
   end Add_Field;

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

   -----------
   -- Field --
   -----------

   function Field
     (Node : Root_Node_Type'Class;
      Name : String)
      return Concorde.Network.Expressions.Expression_Type
   is
   begin
      return Node.Fields.Element (Name);
   end Field;

   ---------------------
   -- Get_Field_Value --
   ---------------------

   overriding function Get_Field_Value
     (State : Node_State_Map;
      Name  : String)
      return Expression_Value
   is
   begin
      return To_Expression_Value (State.Node (Name));
   end Get_Field_Value;

   ---------------
   -- Get_Value --
   ---------------

   overriding function Get_Value
     (State : Node_State_Map)
      return Expression_Value
   is
   begin
      return To_Expression_Value (Non_Negative_Real (State.Map.Length));
   end Get_Value;

   ---------------
   -- Has_Field --
   ---------------

   overriding function Has_Field
     (State : Node_State_Map;
      Name  : String)
      return Boolean
   is
   begin
      return State.Map.Contains (Name);
   end Has_Field;

   ---------------
   -- Has_Field --
   ---------------

   function Has_Field
     (Node : Root_Node_Type'Class;
      Name : String)
      return Boolean
   is
   begin
      return Node.Fields.Contains (Name);
   end Has_Field;

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
      if not State.Map.Contains (Name) then
         raise Constraint_Error with
           "no such state node: " & Name;
      end if;
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
      if not Map.Map.Contains (Name) then
         raise Constraint_Error with
           "no such node: " & Name;
      end if;
      return Map.Map.Element (Name);
   end Node;

   ------------------
   -- Scan_Effects --
   ------------------

   procedure Scan_Effects
     (Node    : Root_Node_Type'Class;
      Env     : Network_State_Interface'Class;
      Process : not null access
        procedure (Target_Node  : Node_State_Access;
                   Effect_Delay : Duration;
                   Effect       : Expressions.Expression_Type))
   is
   begin
      for Effect of Node.Effects loop
         Process (Env.Node (Effect.Target.all),
                  Effect.Effect_Delay, Effect.Expression);
      end loop;
   end Scan_Effects;

   -----------------
   -- Scan_Fields --
   -----------------

   procedure Scan_Fields
     (Node    : Root_Node_Type'Class;
      Process : not null access
        procedure (Field_Name : String;
                   Definition : Concorde.Network.Expressions.Expression_Type))
   is
   begin
      for Position in Node.Fields.Iterate loop
         Process (Field_Maps.Key (Position), Field_Maps.Element (Position));
      end loop;
   end Scan_Fields;

   ----------------------
   -- Scan_State_Nodes --
   ----------------------

   overriding procedure Scan_State_Nodes
     (State   : Node_State_Map;
      Process : not null access
        procedure (Node_State : Node_State_Access))
   is

   begin

      for St of State.Map loop
         Process (St);
      end loop;
   end Scan_State_Nodes;

end Concorde.Network.Nodes;
