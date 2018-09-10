with Ada.Characters.Handling;

with Concorde.Elementary_Functions;

package body Concorde.Network.Expressions is

   function To_Operator (Name : String) return Operator_Type;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Expression     : Expression_Type'Class;
      Env            : Network_State_Interface'Class;
      Argument_Name  : String;
      Argument_Value : Real)
      return Real
   is
      function Eval
        (Node  : Expression_Node)
         return Real;

      ----------
      -- Eval --
      ----------

      function Eval
        (Node  : Expression_Node)
         return Real
      is
      begin
         if Node = null then
            return 0.0;
         end if;

         case Node.Node_Type is
            when Constant_Node =>
               return Node.Constant_Value;
            when Variable_Node =>
               if Node.Variable_Name.all = Argument_Name then
                  return Argument_Value;
               else
                  return Env.Node (Node.Variable_Name.all).Current_Value;
               end if;

            when Operator_Node =>
               declare
                  use Concorde.Elementary_Functions;
                  Left  : constant Real := Eval (Node.Left);
                  Right : constant Real := Eval (Node.Right);
               begin
                  case Node.Operator is
                     when Negate =>
                        return -Left;
                     when Add =>
                        return Left + Right;
                     when Subtract =>
                        return Left - Right;
                     when Multiply =>
                        return Left * Right;
                     when Divide =>
                        return Left / Right;
                     when Power =>
                        return Left ** Right;
                     when Square_Root =>
                        return Sqrt (Left);
                  end case;
               end;
            when Parenthesis_Node =>
               raise Constraint_Error with
                 "cannot evaluate parenthesis node";
         end case;
      end Eval;

   begin
      return Eval (Expression.Root);
   end Evaluate;

   ----------------------
   -- Left_Parenthesis --
   ----------------------

   function Left_Parenthesis return Expression_Node is
   begin
      return new Expression_Node_Record'
        (Node_Type      => Parenthesis_Node,
         Parenthesis    => '(');
   end Left_Parenthesis;

   ----------
   -- Make --
   ----------

   function Make (Text : String) return Expression_Node is
   begin
      if Text = "" then
         return null;
      end if;

      case Text (Text'First) is
         when '0' .. '9' =>
            return new Expression_Node_Record'
              (Node_Type      => Constant_Node,
               Constant_Value => Real'Value (Text));
         when 'a' .. 'z' | 'A' .. 'Z' | '_' =>
            return new Expression_Node_Record'
              (Node_Type      => Variable_Node,
               Variable_Name  =>
                  new String'(Ada.Characters.Handling.To_Lower (Text)));
         when others =>
            return new Expression_Node_Record'
              (Node_Type      => Operator_Node,
               Operator       => To_Operator (Text),
               Left           => null,
               Right          => null);
      end case;
   end Make;

   --------
   -- Op --
   --------

   function Op (Operator : Operator_Type) return Expression_Node is
   begin
      return new Expression_Node_Record'
        (Node_Type      => Operator_Node,
         Operator       => Operator,
         Left           => null,
         Right          => null);
   end Op;

   -----------------------
   -- Right_Parenthesis --
   -----------------------

   function Right_Parenthesis return Expression_Node is
   begin
      return new Expression_Node_Record'
        (Node_Type      => Parenthesis_Node,
         Parenthesis    => ')');
   end Right_Parenthesis;

   -----------------
   -- To_Operator --
   -----------------

   function To_Operator (Name : String) return Operator_Type is
   begin
      if Name = "+" then
         return Add;
      elsif Name = "-" then
         return Subtract;
      elsif Name = "*" then
         return Multiply;
      elsif Name = "/" then
         return Divide;
      elsif Name = "**" then
         return Power;
      else
         raise Constraint_Error with "unknown operator: " & Name;
      end if;
   end To_Operator;

end Concorde.Network.Expressions;
