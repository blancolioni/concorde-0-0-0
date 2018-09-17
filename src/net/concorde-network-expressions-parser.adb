--  with Ada.Containers.Doubly_Linked_Lists;
--  with Ada.Text_IO;

with Concorde.Network.Expressions.Tokens;
with Concorde.Network.Expressions.Lexical;

package body Concorde.Network.Expressions.Parser is

   use Tokens, Lexical;

   subtype Operator_Token is Token range Tok_Plus .. Tok_Power;

   type Precedence_Range is range 1 .. 10;

   Is_Operator : constant array (Token) of Boolean :=
                   (Operator_Token => True, others => False);

   Precedence : constant array (Token) of Precedence_Range :=
                  (Tok_Plus     => 4, Tok_Minus => 4,
                   Tok_Multiply => 6, Tok_Divide => 6,
                   Tok_Power    => 8, others => 1);
   To_Primitive : constant array (Token) of Primitive_Type :=
                    (Tok_Plus     => Add, Tok_Minus => Subtract,
                     Tok_Multiply => Multiply, Tok_Divide => Divide,
                     Tok_Power    => Power,
                     others => Negate);

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
      On_Enter  : not null access
        procedure (Field_Name : String);
      On_Leave  : not null access
        procedure (Field_Name : String);
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
               On_Config (Field_Name, E);
            end;
         elsif Tok = Tok_Left_Brace then
            Scan;
            On_Enter (Field_Name);
            while Tok /= Tok_Right_Brace loop
               Parse (Base & Field_Name & ".");
            end loop;
            On_Leave (Field_Name);
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

      function At_Primary return Boolean
      is (Tok in Tok_Left_Paren | Tok_Identifier | Operator_Token
            | Tok_Sum | Tok_Max | Tok_Min
              | Tok_Integer_Constant | Tok_Float_Constant);

      function Parse_Operator (Prec : Precedence_Range) return Expression_Node;
      function Parse_Primary return Expression_Node;

      --------------------
      -- Parse_Operator --
      --------------------

      function Parse_Operator
        (Prec : Precedence_Range)
         return Expression_Node
      is
         Result      : Expression_Node;
         Have_Prefix : Boolean := False;
         Prefix      : Primitive_Type;
      begin
         if Is_Operator (Tok)
           and then Precedence (Tok) = Prec
         then
            Have_Prefix := True;
            if Tok = Tok_Minus then
               Prefix := Negate;
            else
               Error ("invalid prefix operator");
               raise Constraint_Error;
            end if;
            Scan;
         end if;

         if Prec = Precedence_Range'Last then
            Result := Parse_Primary;
         else
            Result := Parse_Operator (Prec + 1);
         end if;

         while Tok_Indent >= Indent
           and then Is_Operator (Tok)
           and then Precedence (Tok) = Prec
         loop
            declare
               Op    : constant Primitive_Type := To_Primitive (Tok);
               Right : Expression_Node;
            begin
               Scan;
               if Prec = Precedence_Range'Last then
                  Right := Parse_Primary;
               else
                  Right := Parse_Operator (Prec + 1);
               end if;
               Result := Prim (Op, Result, Right);
            end;
         end loop;

         if Have_Prefix then
            return Prim (Prefix, Result);
         else
            return Result;
         end if;
      end Parse_Operator;

      -------------------
      -- Parse_Primary --
      -------------------

      function Parse_Primary return Expression_Node is
         Primary : Expression_Node;
      begin
         if Tok = Tok_Identifier then
            if Next_Tok = Tok_Vertical_Bar then
               declare
                  Constraint : constant Expression_Node :=
                                 new Expression_Node_Record (Constraint_Node);
               begin
                  Constraint.Constraint_Class := new String'(Tok_Text);
                  Scan;
                  Scan;
                  if Tok /= Tok_Identifier then
                     Error ("expected constraint name");
                     raise Constraint_Error;
                  end if;

                  Constraint.Constraint_Name := new String'(Tok_Text);
                  Scan;

                  if Tok = Tok_Arrow then
                     Scan;
                     if Tok /= Tok_Identifier then
                        Error ("expected constraint value");
                        raise Constraint_Error;
                     end if;
                     Constraint.Constraint_Value := new String'(Tok_Text);
                     Scan;
                  end if;
                  Primary := Constraint;
               end;
            else
               Primary := new Expression_Node_Record'
                 (Node_Type        => Variable_Node,
                  Variable_Name    => new String'(Tok_Text));
               Scan;
            end if;
         elsif Tok = Tok_Sum then
            Scan;
            declare
               Arg : constant Expression_Node := Parse_Primary;
            begin
               Primary := Prim (Sum, Arg);
            end;
         elsif Tok = Tok_Min or else Tok = Tok_Max then
            declare
               Prim : constant Primitive_Type :=
                        (if Tok = Tok_Min then Min else Max);
               Exprs : Expression_Node_Lists.List;
            begin
               Scan;
               while At_Primary loop
                  Exprs.Append (Parse_Primary);
               end loop;
               Primary := new Expression_Node_Record'
                 (Primitive_Node, Prim, Exprs);
            end;
         elsif Tok = Tok_Integer_Constant
           or else Tok = Tok_Float_Constant
         then
            Primary := new Expression_Node_Record'
              (Node_Type      => Constant_Node,
               Constant_Value => Real'Value (Tok_Text));
            Scan;
         elsif Tok = Tok_Left_Paren then
            Scan;
            Primary := Parse_Operator (Precedence_Range'First);
            if Tok = Tok_Right_Paren then
               Scan;
            else
               Error ("missing ')'");
            end if;
         else
            raise Constraint_Error with "syntax error at " & Tok_Text;
         end if;

         while Tok_Indent >= Indent
           and then Tok = Tok_Dot
         loop
            if Next_Tok /= Tok_Identifier then
               Error ("missing field name");
               raise Constraint_Error;
            end if;
            Scan;

            declare
               Selector : constant Expression_Node :=
                            new Expression_Node_Record'
                              (Node_Type        => Field_Selector_Node,
                               Field_Container  => Primary,
                               Field_Name       => new String'(Tok_Text));
            begin
               Scan;
               Primary := Selector;
            end;
         end loop;
         return Primary;
      end Parse_Primary;

   begin
      return Parse_Operator (Precedence_Range'First);
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
