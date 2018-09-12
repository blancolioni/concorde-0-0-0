with Concorde.Elementary_Functions;

package body Concorde.Network.Expressions is

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

      No_Values : Array_Of_Values (1 .. 0);

      function V (X : Real) return Array_Of_Values
      is (1 => To_Expression_Value (X));

      function Eval
        (Node  : Expression_Node)
         return Array_Of_Values;

      ----------
      -- Eval --
      ----------

      function Eval
        (Node  : Expression_Node)
         return Array_Of_Values
      is
      begin
         if Node = null then
            return No_Values;
         end if;

         case Node.Node_Type is
            when Constant_Node =>
               return V (Node.Constant_Value);
            when Variable_Node =>
               if Node.Variable_Name.all = Argument_Name then
                  return V (Argument_Value);
               else
                  return V (Env.Node (Node.Variable_Name.all).Current_Value);
               end if;
            when Constraint_Node =>
               return Env.Evaluate_Constraint
                 (Class_Name       => Node.Constraint_Class.all,
                  Constraint_Name  => Node.Constraint_Name.all,
                  Constraint_Value => Node.Constraint_Value.all);
            when Field_Selector_Node =>
               declare
                  Xs : Array_Of_Values := Eval (Node.Field_Container);
               begin
                  for X of Xs loop
                     if X.Object_Value /= null then
                        X :=
                          X.Object_Value.Get_Field_Value
                            (Node.Field_Name.all);
                     end if;
                  end loop;
                  return Xs;
               end;
            when Primitive_Node =>
               declare
                  use type Ada.Containers.Count_Type;
                  use Concorde.Elementary_Functions;
                  Left  : constant Array_Of_Values :=
                            Eval (Node.Prim_Args.First_Element);
                  Right : constant Array_Of_Values :=
                            (if Node.Prim_Args.Length > 1
                             then Eval
                               (Expression_Node_Lists.Element
                                  (Expression_Node_Lists.Next
                                     (Node.Prim_Args.First)))
                             else No_Values);
                  Xs    : Array_Of_Values (Left'Range);
                  S     : Real := 0.0;
               begin
                  for I in Xs'Range loop
                     declare
                        L : constant Real := Left (I).Real_Value;
                        R : constant Real :=
                              (if I in Right'Range
                               then Right (I).Real_Value
                               elsif Node.Primitive = Divide
                               or else Node.Primitive = Multiply
                               then 1.0
                               else 0.0);
                        X : Real;
                     begin
                        case Node.Primitive is
                           when Negate =>
                              X := -L;
                           when Add =>
                              X := L + R;
                           when Subtract =>
                              X := L - R;
                           when Multiply =>
                              X := L * R;
                           when Divide =>
                              X := L / R;
                           when Power =>
                              X := L ** R;
                           when Square_Root =>
                              X := Sqrt (L);
                           when Sum =>
                              X := L;
                        end case;
                        if Node.Primitive = Sum then
                           S := S + X;
                        else
                           Xs (I) := To_Expression_Value (X);
                        end if;
                     end;
                  end loop;
                  if Node.Primitive = Sum then
                     return V (S);
                  else
                     return Xs;
                  end if;
               end;
         end case;
      end Eval;

      Xs : constant Array_Of_Values := Eval (Expression.Root);

   begin
      if Xs'Length = 0 then
         return 0.0;
      else
         return Xs (Xs'First).Real_Value;
      end if;
   end Evaluate;

   ----------
   -- Prim --
   ----------

   function Prim (Primitive : Primitive_Type;
                  Arg_1     : Expression_Node := null;
                  Arg_2     : Expression_Node := null;
                  Arg_3     : Expression_Node := null)
                  return Expression_Node
   is
      Args : constant array (Positive range <>) of Expression_Node :=
               (Arg_1, Arg_2, Arg_3);
   begin
      return Node : constant Expression_Node := new Expression_Node_Record'
        (Node_Type => Primitive_Node,
         Primitive => Primitive,
         Prim_Args => <>)
      do
         for Arg of Args loop
            exit when Arg = null;
            Node.Prim_Args.Append (Arg);
         end loop;
      end return;
   end Prim;

end Concorde.Network.Expressions;
