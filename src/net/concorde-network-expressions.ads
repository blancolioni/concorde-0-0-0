private with Ada.Containers.Doubly_Linked_Lists;

package Concorde.Network.Expressions is

   type Expression_Type is tagged private;

   function Evaluate
     (Expression     : Expression_Type'Class;
      Env            : Network_State_Interface'Class;
      Argument_Name  : String;
      Argument_Value : Real)
      return Real;

   function Evaluate (Expression : Expression_Type'Class) return Real;

private

   type Expression_Node_Type is
     (Constant_Node, Variable_Node, Primitive_Node,
      Constraint_Node, Field_Selector_Node);

   type Primitive_Type is
     (Negate, Add, Subtract, Multiply, Divide, Power, Square_Root, Sum);

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

end Concorde.Network.Expressions;
