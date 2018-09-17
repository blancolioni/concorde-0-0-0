private with Ada.Containers.Doubly_Linked_Lists;
private with WL.String_Maps;

package Concorde.Network.Expressions is

   type Local_Environment is tagged private;

   procedure Add
     (Env  : in out Local_Environment'Class;
      Name : String;
      Value : Real);

   type Expression_Type is tagged private;

   function Show
     (Expression : Expression_Type'Class)
      return String;

   function Evaluate
     (Expression     : Expression_Type'Class;
      State          : Network_State_Interface'Class;
      Local_Env      : Local_Environment'Class)
      return Real;

   function Evaluate
     (Expression : Expression_Type'Class;
      State      : Network_State_Interface'Class;
      Current    : Expression_Object_Interface'Class)
      return Real;

   function Evaluate
     (Expression : Expression_Type'Class;
      State      : Network_State_Interface'Class;
      Local_Env  : Local_Environment'Class;
      Current    : Expression_Object_Interface'Class)
      return Real;

   function Evaluate
     (Expression     : Expression_Type'Class;
      Env            : Network_State_Interface'Class;
      Argument_Name  : String;
      Argument_Value : Real)
      return Real;

   function Evaluate
     (Expression     : Expression_Type'Class;
      Env            : Network_State_Interface'Class)
      return Real;

   function Evaluate (Expression : Expression_Type'Class) return Real;

private

   type Expression_Node_Type is
     (Constant_Node, Variable_Node, Primitive_Node,
      Constraint_Node, Field_Selector_Node);

   type Primitive_Type is
     (Negate, Add, Subtract, Multiply, Divide, Power, Square_Root,
      Sum, Max, Min);

   type Expression_Node_Record (Node_Type : Expression_Node_Type);

   type Expression_Node is access Expression_Node_Record;

   package Expression_Node_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Expression_Node);

   type Expression_Node_Record (Node_Type : Expression_Node_Type) is
      record
         case Node_Type is
            when Constant_Node =>
               Constant_Value : Real;
            when Variable_Node =>
               Variable_Name  : access String;
            when Primitive_Node =>
               Primitive      : Primitive_Type;
               Prim_Args      : Expression_Node_Lists.List;
            when Constraint_Node =>
               Constraint_Class : access String;
               Constraint_Name  : access String;
               Constraint_Value : access String;
            when Field_Selector_Node =>
               Field_Container  : Expression_Node;
               Field_Name       : access String;
         end case;
      end record;

   function Prim (Primitive : Primitive_Type;
                  Arg_1     : Expression_Node := null;
                  Arg_2     : Expression_Node := null;
                  Arg_3     : Expression_Node := null)
                  return Expression_Node;

   type Expression_Type is tagged
      record
         Root : Expression_Node;
      end record;

   package Local_Env_Maps is
     new WL.String_Maps (Real);

   type Local_Environment is tagged
      record
         Map : Local_Env_Maps.Map;
      end record;

end Concorde.Network.Expressions;
