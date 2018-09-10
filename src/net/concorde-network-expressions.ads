package Concorde.Network.Expressions is

   type Expression_Type is tagged private;

   function Evaluate
     (Expression     : Expression_Type'Class;
      Env            : Network_State_Interface'Class;
      Argument_Name  : String;
      Argument_Value : Real)
      return Real;

private

   type Expression_Node_Type is
     (Constant_Node, Variable_Node, Operator_Node, Parenthesis_Node);

   type Operator_Type is
     (Negate, Add, Subtract, Multiply, Divide, Power, Square_Root);

   type Expression_Node_Record (Node_Type : Expression_Node_Type);

   type Expression_Node is access Expression_Node_Record;

   type Parenthesis_Type is ('(', ')');

   type Expression_Node_Record (Node_Type : Expression_Node_Type) is
      record
         case Node_Type is
            when Constant_Node =>
               Constant_Value : Real;
            when Variable_Node =>
               Variable_Name  : access String;
            when Operator_Node =>
               Operator       : Operator_Type;
               Left, Right    : Expression_Node;
            when Parenthesis_Node =>
               Parenthesis    : Parenthesis_Type;
         end case;
      end record;

   function Make (Text : String) return Expression_Node;
   function Op (Operator : Operator_Type) return Expression_Node;
   function Left_Parenthesis return Expression_Node;
   function Right_Parenthesis return Expression_Node;

   type Expression_Type is tagged
      record
         Root : Expression_Node;
      end record;

end Concorde.Network.Expressions;
