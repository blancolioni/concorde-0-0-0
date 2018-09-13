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

   -----------------------
   -- Run_Network_State --
   -----------------------

   overriding procedure Run_Network_State
     (State : in out Node_State_Map)
   is

   begin

      for St of State.Map loop
         declare
            S : constant Node_State_Access :=
                  State.Node (St.Identifier);

            procedure Send_Effect
              (Target_Node  : Node_State_Access;
               Effect_Delay : Duration;
               Effect       : Expressions.Expression_Type);

            -----------------
            -- Send_Effect --
            -----------------

            procedure Send_Effect
              (Target_Node  : Node_State_Access;
               Effect_Delay : Duration;
               Effect       : Expressions.Expression_Type)
            is
            begin
               Target_Node.Add_Effect
                 (Concorde.Network.Expressions.Evaluate
                    (Expression     => Effect,
                     Env            => State,
                     Argument_Name  => "x",
                     Argument_Value =>
                       S.Current_Inertial_Value (Effect_Delay)));
            end Send_Effect;

         begin

            Node (St.Identifier).Scan_Effects (State, Send_Effect'Access);

         end;

      end loop;

      for St of State.Map loop
         St.Set_New_Value;
      end loop;
   end Run_Network_State;

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

end Concorde.Network.Nodes;
