private with Ada.Containers.Doubly_Linked_Lists;
private with WL.String_Maps;

with Concorde.Calendar;
with Concorde.Network.Nodes;

package Concorde.Network.State is

   function New_Internal_Node_State
     (Node       : not null access constant
        Concorde.Network.Nodes.Root_Node_Type'Class)
      return Node_State_Access;

   type Root_Node_State_Type is
     new Node_State_Interface
     and Expression_Object_Interface
   with private;

   procedure Initialize_State
     (State : in out Root_Node_State_Type'Class;
      Node  : not null access constant
        Concorde.Network.Nodes.Root_Node_Type'Class;
      Init  : Real);

   overriding function Identifier
     (Node_State : Root_Node_State_Type) return String;

   overriding function Current_Value
     (Node_State : Root_Node_State_Type) return Signed_Unit_Real;

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

   overriding procedure Send_Effects
     (Node_State : in out Root_Node_State_Type;
      Network_State : Network_State_Interface'Class);

   overriding procedure Add_Effect
     (Node_State : in out Root_Node_State_Type;
      Effect     : Signed_Unit_Real);

   overriding procedure Set_New_Value
     (Node_State    : in out Root_Node_State_Type;
      Network_State : Network_State_Interface'Class);

   overriding function Current_Inertial_Value
     (Node    : Root_Node_State_Type;
      Inertia : Duration)
      return Real;

   function Node (State : Root_Node_State_Type'Class)
                  return Concorde.Network.Nodes.Node_Type;

private

   type Historical_Value is
      record
         Date   : Concorde.Calendar.Time;
         Value  : Unit_Real;
      end record;

   package Historical_Value_List is
     new Ada.Containers.Doubly_Linked_Lists (Historical_Value);

   package Field_Maps is
     new WL.String_Maps (Real);

   type Root_Node_State_Type is
     new Node_State_Interface
     and Expression_Object_Interface with
      record
         Node         : Concorde.Network.Nodes.Node_Type;
         History      : Historical_Value_List.List;
         Current_Bias : Signed_Unit_Real := 0.0;
         Fields       : Field_Maps.Map;
         New_Value    : Real      := 0.0;
         Active       : Boolean   := True;
         Changed      : Boolean   := False;
      end record;

   overriding function Get_Value
     (Node_State : Root_Node_State_Type)
      return Expression_Value;

   overriding function Has_Field
     (Node_State : Root_Node_State_Type;
      Name       : String)
      return Boolean;

   overriding function Get_Field_Value
     (Node_State : Root_Node_State_Type;
      Name  : String)
      return Expression_Value;

   overriding function Identifier
     (Node_State : Root_Node_State_Type) return String
   is (Node_State.Node.Identifier);

   function Node (State : Root_Node_State_Type'Class)
                  return Concorde.Network.Nodes.Node_Type
   is (State.Node);

end Concorde.Network.State;
