private with Ada.Containers.Doubly_Linked_Lists;
private with WL.String_Maps;

with Concorde.Objects;
with Concorde.Network.Expressions;

package Concorde.Network.Nodes is

   type Root_Node_Type is
     abstract new Concorde.Objects.Root_Localised_Object_Type with private;

   subtype Root_Node_Class is Root_Node_Type'Class;
   type Node_Type is access constant Root_Node_Type'Class;

   procedure Scan_Inputs
     (Node    : Root_Node_Type'Class;
      Process : not null access
        procedure (Input_Node : Node_Type;
                   Input_Inertia : Duration;
                   Expression : Concorde.Network.Expressions.Expression_Type));

   procedure Add_Node
     (Node : Node_Type);

   function Node
     (Name    : String)
      return Node_Type;

   type Node_State_Map is new Network_State_Interface with private;

private

   type Node_Input is
      record
         Source     : Node_Type;
         Expression : Concorde.Network.Expressions.Expression_Type;
         Inertia    : Duration;
      end record;

   package Node_Input_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Node_Input);

   type Root_Node_Type is
     abstract new Concorde.Objects.Root_Localised_Object_Type with
      record
         Current_Value : Non_Negative_Real := 0.0;
         Inputs        : Node_Input_Lists.List;
      end record;

--     procedure Set_Outputs
--       (Node  : in out Root_Node_Type'Class;
--        State : in out Concorde.Network.Expressions.Environment'Class);
--
--     procedure Update_From_Inputs
--       (Node  : in out Root_Node_Type'Class);

   package Node_Maps is
     new WL.String_Maps (Node_Type);

   type Node_Network is tagged
      record
         Map : Node_Maps.Map;
      end record;

   package Node_State_Maps is
     new WL.String_Maps (Node_State_Access);

   type Node_State_Map is new Network_State_Interface with
      record
         Map : Node_State_Maps.Map;
         Env : Network_State_Access;
      end record;

   overriding function Node
     (State : Node_State_Map;
      Name  : String)
      return Node_State_Access;

   overriding procedure Update
     (State : in out Node_State_Map);

--     function Node
--       (Map  : Node_Map'Class;
--        Name : String)
--        return Node_Type
--     is (if Map.Map.Contains (Name)
--         then Map.Map.Element (Name)
--         else raise Constraint_Error with "no such node: " & Name);

end Concorde.Network.Nodes;
