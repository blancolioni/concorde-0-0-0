with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

with Concorde.Network.Expressions.Tokens;
with Concorde.Network.Expressions.Lexical;

package body Concorde.Network.Expressions.Parser is

   use Tokens, Lexical;

   subtype Operator_Token is Token range Tok_Plus .. Tok_Power;

   Precedence : constant array (Operator_Token) of Natural :=
                  (Tok_Plus     => 4, Tok_Minus => 4,
                   Tok_Multiply => 6, Tok_Divide => 6,
                   Tok_Power    => 8);
   To_Primitive : constant array (Operator_Token) of Primitive_Type :=
                    (Tok_Plus     => Add, Tok_Minus => Subtract,
                     Tok_Multiply => Multiply, Tok_Divide => Divide,
                     Tok_Power    => Power);

   procedure Error (Message : String;
                    Expr    : String)
     with Unreferenced;

   function Parse_Expression_Line
     return Expression_Node;

   -----------
   -- Error --
   -----------

   procedure Error (Message : String;
                    Expr    : String)
   is
   begin
      raise Constraint_Error with
        "error: " & Message & " in: " & Expr;
   end Error;

   -------------------------
   -- Parse_Configuration --
   -------------------------

   procedure Parse_Configuration
     (Path      : String;
      On_Config : not null access
        procedure (Field_Name : String;
                   Field_Value : Expression_Type))
   is
      procedure Parse (Base : String);

      -----------
      -- Parse --
      -----------

      procedure Parse (Base : String) is
         Field_Name : constant String := Tok_Text;
      begin
         if Tok /= Tok_Identifier then
            Error ("missing identifier");
            raise Constraint_Error;
         end if;

         Scan;

         if Tok = Tok_Colon then
            Scan;
            declare
               E : constant Expression_Type :=
                     Expression_Type'
                       (Root => Parse_Expression_Line);
            begin
               On_Config (Base & Field_Name, E);
            end;
         elsif Tok = Tok_Left_Brace then
            Scan;
            while Tok /= Tok_Right_Brace loop
               Parse (Base & Field_Name & ".");
            end loop;
            Scan;
         else
            Error ("syntax error");
            raise Constraint_Error;
         end if;
      end Parse;

   begin
      Open (Path);
      while Tok /= Tok_End_Of_File loop
         Parse ("");
      end loop;
      Close;
   end Parse_Configuration;

   ---------------------------
   -- Parse_Expression_Line --
   ---------------------------

   function Parse_Expression_Line
     return Expression_Node
   is

      Indent : constant Positive := Tok_Indent;

      package Stacks is
        new Ada.Containers.Doubly_Linked_Lists (Token);

      package Exprs is
        new Ada.Containers.Doubly_Linked_Lists (Expression_Node);

      Output : Exprs.List;
      Stack  : Stacks.List;

      procedure Pop_Operator;

      ------------------
      -- Pop_Operator --
      ------------------

      procedure Pop_Operator is
         Op : constant Token := Stack.First_Element;
         Left, Right : Expression_Node;
         Expr        : Expression_Node;
      begin
         Stack.Delete_First;
         Right := Output.Last_Element;
         Output.Delete_Last;

         if Output.Is_Empty then
            if Op = Tok_Minus then
               Expr := Prim (Negate, Right);
            else
               raise Constraint_Error with
                 "operator: missing argument";
            end if;
         else
            Left := Output.Last_Element;
            Output.Delete_Last;
            Expr := Prim (To_Primitive (Op), Left, Right);
         end if;

         Output.Append (Expr);

      end Pop_Operator;

   begin

      while Tok_Indent >= Indent loop
         Ada.Text_IO.Put_Line (Token'Image (Tok) & " " & Tok_Text);
         if Tok = Tok_Identifier then
            Output.Append (new Expression_Node_Record'
                             (Node_Type        => Variable_Node,
                              Variable_Name    => new String'(Tok_Text)));
            Scan;
         elsif Tok = Tok_Integer_Constant
           or else Tok = Tok_Float_Constant
         then
            Output.Append (new Expression_Node_Record'
                             (Node_Type      => Constant_Node,
                              Constant_Value => Real'Value (Tok_Text)));
            Scan;
         elsif Tok = Tok_Dot then
            if Next_Tok /= Tok_Identifier then
               Error ("missing field name");
               raise Constraint_Error;
            end if;
            Scan;

            declare
               Selector : constant Expression_Node :=
                            new Expression_Node_Record'
                              (Node_Type        => Field_Selector_Node,
                               Field_Container  => Output.Last_Element,
                               Field_Name       => new String'(Tok_Text));
            begin
               Scan;
               Output.Delete_Last;
               Output.Append (Selector);
            end;
         elsif Tok in Operator_Token then
            while not Stack.Is_Empty
              and then Stack.First_Element in Operator_Token
              and then Precedence (Stack.First_Element)
              >= Precedence (Tok)
            loop
               Pop_Operator;
            end loop;
            Stack.Insert (Stack.First, Tok);
            Scan;
         elsif Tok = Tok_Left_Paren then
            Stack.Insert (Stack.First, Tok);
            Scan;
         elsif Tok = Tok_Right_Paren then
            while not Stack.Is_Empty
              and then Stack.First_Element /= Tok_Left_Paren
            loop
               Pop_Operator;
            end loop;
            if Stack.Is_Empty then
               raise Constraint_Error with "mismatched parentheses";
            else
               Stack.Delete_First;
            end if;
            Scan;
         else
            raise Constraint_Error with "syntax error at " & Tok_Text;
         end if;
      end loop;

      while not Stack.Is_Empty loop
         Pop_Operator;
      end loop;

      return Output.First_Element;
   end Parse_Expression_Line;

   ------------------
   -- Parse_String --
   ------------------

   function Parse_String
     (Text : String)
      return Expression_Type
   is
   begin
      return Expr : Expression_Type do
         Open_String (Text);
         Expr.Root := Parse_Expression_Line;
         Close;
      end return;
   end Parse_String;

end Concorde.Network.Expressions.Parser;
