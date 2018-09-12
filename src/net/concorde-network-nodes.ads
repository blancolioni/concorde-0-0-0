private with Ada.Containers.Doubly_Linked_Lists;
private with WL.String_Maps;

with Concorde.Network.Expressions;

package Concorde.Network.Nodes is

   type Root_Node_Type is
     abstract tagged private;

   function Identifier (Node : Root_Node_Type) return String;

   procedure Initialise
     (Node : in out Root_Node_Type'Class;
      Id   : String);

   subtype Root_Node_Class is Root_Node_Type'Class;
   type Node_Type is access constant Root_Node_Type'Class;

   function Create_State
     (Node          : not null access constant Root_Node_Type;
      Initial_Value : Real)
      return Node_State_Access;

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

   type Root_Node_Type is abstract tagged
      record
         Id     : access String;
         Inputs : Node_Input_Lists.List;
      end record;

   function Identifier (Node : Root_Node_Type) return String
   is (Node.Id.all);

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

   overriding procedure Add_Node
     (State : in out Node_State_Map;
      Node  : Node_State_Access);

   overriding function Node
     (State : Node_State_Map;
      Name  : String)
      return Node_State_Access;

   overriding procedure Update
     (State : in out Node_State_Map);

   overriding function Evaluate_Constraint
     (From             : Node_State_Map;
      Class_Name       : String;
      Constraint_Name  : String;
      Constraint_Value : String)
      return Array_Of_Values;

end Concorde.Network.Nodes;
