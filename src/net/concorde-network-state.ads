private with Ada.Containers.Doubly_Linked_Lists;

with Concorde.Calendar;
with Concorde.Network.Nodes;

package Concorde.Network.State is

   function New_Internal_Node_State
     (Node       : not null access constant
        Concorde.Network.Nodes.Root_Node_Type'Class)
      return Node_State_Access;

   type Root_Node_State_Type is new Node_State_Interface with private;

   procedure Initialize_State
     (State : in out Root_Node_State_Type'Class;
      Node  : not null access constant
        Concorde.Network.Nodes.Root_Node_Type'Class;
      Init  : Real);

   overriding function Identifier
     (Node_State : Root_Node_State_Type) return String;

   overriding function Current_Value
     (Node_State : Root_Node_State_Type) return Unit_Real;

   overriding function Current_Actual_Value
     (Node_State : Root_Node_State_Type) return Real;

   overriding function Current_Base_Value
     (Node_State : Root_Node_State_Type) return Real;

   overriding function Show_Value
     (Node_State : Root_Node_State_Type) return String;

   overriding function Is_Active
     (Node_State : Root_Node_State_Type) return Boolean;

   overriding procedure Set_Initial_Value
     (Node_State    : in out Root_Node_State_Type;
      Value         : Real);

   overriding procedure Add_Effect
     (Node_State : in out Root_Node_State_Type;
      Effect     : Signed_Unit_Real);

   overriding procedure Set_New_Value
     (Node_State : in out Root_Node_State_Type);

   overriding function Current_Inertial_Value
     (Node    : Root_Node_State_Type;
      Inertia : Duration)
      return Real;

private

   type Historical_Value is
      record
         Date   : Concorde.Calendar.Time;
         Value  : Unit_Real;
      end record;

   package Historical_Value_List is
     new Ada.Containers.Doubly_Linked_Lists (Historical_Value);

   type Root_Node_State_Type is new Node_State_Interface with
      record
         Node          : Concorde.Network.Nodes.Node_Type;
         History       : Historical_Value_List.List;
         Current_Value : Unit_Real := 0.0;
         New_Value     : Unit_Real := 0.0;
         Base_Value    : Real      := 1.0;
         Active        : Boolean   := True;
      end record;

   overriding function Identifier
     (Node_State : Root_Node_State_Type) return String
   is (Node_State.Node.Identifier);

end Concorde.Network.State;
