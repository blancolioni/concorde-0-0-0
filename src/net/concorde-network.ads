package Concorde.Network is

   type Node_State_Interface is limited interface;
   type Node_State_Access is access all Node_State_Interface'Class;

   function Identifier
     (Node_State : Node_State_Interface) return String
      is abstract;

   function Current_Value
     (Node_State : Node_State_Interface) return Signed_Unit_Real
      is abstract;

   function Current_Actual_Value (Node : Node_State_Interface) return Real
                                  is abstract;

   function Current_Base_Value (Node : Node_State_Interface) return Real
                                is abstract;

   function Current_Inertial_Value
     (Node    : Node_State_Interface;
      Inertia : Duration)
      return Real
      is abstract;

   function Show_Value (Node : Node_State_Interface) return String
                        is abstract;

   procedure After_Update (Node : in out Node_State_Interface)
   is null;

   function Is_Active (Node : Node_State_Interface) return Boolean
   is abstract;

   procedure Set_Initial_Value
     (Node  : in out Node_State_Interface;
      Value : Real)
   is abstract;

   procedure Add_Effect
     (Node : in out Node_State_Interface;
      Value : Signed_Unit_Real)
   is abstract;

   type Network_State_Interface is limited interface;
   type Network_State_Access is access Network_State_Interface'Class;

   function Node
     (State : Network_State_Interface;
      Name  : String)
      return Node_State_Access
      is abstract;

   procedure Add_Node
     (State : in out Network_State_Interface;
      Node  : Node_State_Access)
   is abstract;

   procedure Scan_State_Nodes
     (State : Network_State_Interface;
      Process : not null access
        procedure (Node_State : Node_State_Access))
   is abstract;

   procedure Run_Network_State
     (State : in out Network_State_Interface'Class);

   procedure Send_Effects
     (Node_State : in out Node_State_Interface;
      Network_State : Network_State_Interface'Class)
   is abstract;

   procedure Set_New_Value
     (Node_State    : in out Node_State_Interface;
      Network_State : Network_State_Interface'Class)
   is abstract;

   type Expression_Object_Interface is limited interface;

   type Expression_Value is private;

   function To_Expression_Value (X : Real) return Expression_Value;
   function To_Expression_Value
     (X : not null access constant Expression_Object_Interface'Class)
      return Expression_Value;

   type Array_Of_Values is array (Positive range <>) of Expression_Value;

   type Expression_Object is access constant Expression_Object_Interface'Class;

   function Get_Value
     (Value : Expression_Object_Interface)
      return Expression_Value
   is abstract;

   function Get_Field_Value
     (Value : Expression_Object_Interface;
      Name  : String)
      return Expression_Value
      is abstract;

   function Evaluate_Constraint
     (From             : Network_State_Interface;
      Class_Name       : String;
      Constraint_Name  : String;
      Constraint_Value : String)
      return Array_Of_Values
      is abstract;

private

   type Expression_Value is
      record
         Real_Value   : Real;
         Object_Value : Expression_Object;
      end record;

end Concorde.Network;
