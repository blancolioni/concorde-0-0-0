package Concorde.Network is

   type Node_State_Interface is interface;
   type Node_State_Access is access all Node_State_Interface'Class;

   function Current_Value
     (Node_State : Node_State_Interface) return Unit_Real
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

   type Network_State_Interface is interface;
   type Network_State_Access is access Network_State_Interface'Class;

   function Node
     (State : Network_State_Interface;
      Name  : String)
      return Node_State_Access
      is abstract;

   procedure Update (State : in out Network_State_Interface) is abstract;

   procedure Calculate_New_Value
     (Node_State    : in out Node_State_Interface;
      Network_State : Network_State_Interface'Class)
   is abstract;

   procedure Set_New_Value
     (Node_State : in out Node_State_Interface)
     is abstract;

end Concorde.Network;
