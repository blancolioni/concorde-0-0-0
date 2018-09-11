with Ada.Containers.Doubly_Linked_Lists;

package body Concorde.Network.Expressions.Parser is

   Precedence : constant array (Operator_Type) of Natural :=
                  (3, 3, 3, 4, 4, 5, 5);

   procedure Error (Message : String;
                    Expr    : String);

   package Expression_Node_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Expression_Node);

   procedure Lex
     (Text : String;
      List : in out Expression_Node_Lists.List);

   function Parse
     (Expr : Expression_Node_Lists.List)
      return Expression_Node;

   -----------
   -- Error --
   -----------

   procedure Error (Message : String;
                    Expr    : String)
   is
   begin
      raise Constraint_Error with
        "error: " & Message & " in expression: " & Expr;
   end Error;

   ---------
   -- Lex --
   ---------

   procedure Lex
     (Text : String;
      List : in out Expression_Node_Lists.List)
   is
      Index  : Positive := 1;
      Expr   : constant String := Text & ' ';
      Token  : String (Expr'Range);
      Length : Natural := 0;
      Ch     : Character := Expr (Expr'First);

      procedure Skip;
      procedure Add_Character;

      procedure Save_Token (Node : Expression_Node := null);

      -------------------
      -- Add_Character --
      -------------------

      procedure Add_Character is
      begin
         Length := Length + 1;
         Token (Length) := Ch;
         Skip;
      end Add_Character;

      ----------------
      -- Save_Token --
      ----------------

      procedure Save_Token (Node : Expression_Node := null) is
         N : Expression_Node := Node;
      begin
         if N = null then
            N := Make (Token (1 .. Length));
         end if;
         List.Append (N);
      end Save_Token;

      ----------
      -- Skip --
      ----------

      procedure Skip is
      begin
         Index := Index + 1;
         if Index <= Expr'Last then
            Ch := Expr (Index);
         else
            Ch := Character'Val (0);
         end if;
      end Skip;

   begin

      while Ch = ' ' loop
         Skip;
      end loop;

      while Index <= Text'Last loop
         Length := 0;

         case Ch is
            when '0' .. '9' =>
               while Ch in '0' .. '9' | '.' | '_' | 'e' | 'E' loop
                  Add_Character;
               end loop;
               Save_Token;

            when 'a' .. 'z' | 'A' .. 'Z' | '_' =>
               while Ch in 'a' .. 'z' | 'A' .. 'Z'
                   | '0' .. '9'
                   | '_' | '-'
               loop
                  Add_Character;
               end loop;
               Save_Token;

            when '(' =>
               Save_Token (Left_Parenthesis);
               Skip;

            when ')' =>
               Save_Token (Right_Parenthesis);
               Skip;

            when '*' =>
               Skip;
               if Ch = '*' then
                  Skip;
                  Save_Token (Op (Power));
               else
                  Save_Token (Op (Multiply));
               end if;

            when '+' =>
               Skip;
               Save_Token (Op (Add));

            when '-' =>
               Skip;
               Save_Token (Op (Subtract));

            when '/' =>
               Skip;
               Save_Token (Op (Divide));

            when others =>
               Error ("bad character '" & Ch & "'", Text);

         end case;

         while Ch = ' ' loop
            Skip;
         end loop;

      end loop;
   end Lex;

   -----------
   -- Parse --
   -----------

   function Parse
     (Expr : Expression_Node_Lists.List)
      return Expression_Node
   is
      Output : Expression_Node_Lists.List;
      Stack  : Expression_Node_Lists.List;

      procedure Pop_Operator;

      ------------------
      -- Pop_Operator --
      ------------------

      procedure Pop_Operator is
         Op : constant Expression_Node :=
                Stack.First_Element;
      begin
         Stack.Delete_First;
         Op.Right := Output.Last_Element;
         Output.Delete_Last;
         if Output.Is_Empty then
            if Op.Operator = Subtract then
               Op.Operator := Negate;
               Op.Left := Op.Right;
               Op.Right := null;
            else
               raise Constraint_Error with
                 "operator: missing argument";
            end if;
         else
            Op.Left := Output.Last_Element;
            Output.Delete_Last;
         end if;

         Output.Append (Op);

      end Pop_Operator;

   begin
      for Node of Expr loop
         case Node.Node_Type is
            when Constant_Node | Variable_Node =>
               Output.Append (Node);
            when Operator_Node =>
               while not Stack.Is_Empty
                 and then Stack.First_Element.Node_Type = Operator_Node
                 and then Precedence (Stack.First_Element.Operator)
                 >= Precedence (Node.Operator)
               loop
                  Pop_Operator;
               end loop;
               Stack.Insert (Stack.First, Node);
            when Parenthesis_Node =>
               case Node.Parenthesis is
                  when '(' =>
                     Stack.Insert (Stack.First, Node);
                  when ')' =>
                     while not Stack.Is_Empty
                       and then Stack.First_Element.Node_Type
                         /= Parenthesis_Node
                     loop
                        Pop_Operator;
                     end loop;
                     if Stack.Is_Empty then
                        raise Constraint_Error with "mismatched parentheses";
                     else
                        Stack.Delete_First;
                     end if;
               end case;
         end case;
      end loop;

      while not Stack.Is_Empty loop
         Pop_Operator;
      end loop;

      return Output.First_Element;
   end Parse;

   ------------------
   -- Parse_String --
   ------------------

   function Parse_String
     (Text : String)
      return Expression_Type
   is
      List : Expression_Node_Lists.List;
   begin
      Lex (Text, List);
      return Expr : Expression_Type do
         Expr.Root := Parse (List);
      end return;
   end Parse_String;

end Concorde.Network.Expressions.Parser;
