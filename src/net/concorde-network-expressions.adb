with Concorde.Elementary_Functions;
with Concorde.Real_Images;

package body Concorde.Network.Expressions is

   type Null_Network_State is new Network_State_Interface with null record;

   overriding function Node
     (State : Null_Network_State;
      Name  : String)
      return Node_State_Access;

   overriding procedure Add_Node
     (State : in out Null_Network_State;
      Node  : Node_State_Access)
   is null;

   overriding procedure Scan_State_Nodes
     (State : Null_Network_State;
      Process : not null access
        procedure (Node : Node_State_Access))
   is null;

   overriding function Evaluate_Constraint
     (From             : Null_Network_State;
      Class_Name       : String;
      Constraint_Name  : String;
      Constraint_Value : String)
      return Array_Of_Values;

   overriding function Get_Value
     (Value : Null_Network_State)
      return Expression_Value;

   overriding function Has_Field
     (Value : Null_Network_State;
      Name  : String)
      return Boolean;

   overriding function Get_Field_Value
     (Value : Null_Network_State;
      Name  : String)
      return Expression_Value;

   ---------
   -- Add --
   ---------

   procedure Add
     (Env   : in out Local_Environment'Class;
      Name  : String;
      Value : Real)
   is
   begin
      Env.Map.Insert (Name, Value);
   end Add;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (Expression : Expression_Type'Class) return Real is
      State : Null_Network_State;
   begin
      return Evaluate (Expression, State, "", 0.0);
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Expression     : Expression_Type'Class;
      Env            : Network_State_Interface'Class)
      return Real
   is
   begin
      return Expression.Evaluate (Env, "", 0.0);
   end Evaluate;

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
      Local : Local_Environment;
   begin
      Local.Add (Argument_Name, Argument_Value);
      return Expression.Evaluate (Env, Local);
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Expression     : Expression_Type'Class;
      State          : Network_State_Interface'Class;
      Local_Env      : Local_Environment'Class)
      return Real
   is
      Object : Null_Network_State;
   begin
      return Expression.Evaluate (State, Local_Env, Object);
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Expression : Expression_Type'Class;
      State      : Network_State_Interface'Class;
      Current    : Expression_Object_Interface'Class)
      return Real
   is
      Local : Local_Environment;
   begin
      return Expression.Evaluate (State, Local, Current);
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Expression : Expression_Type'Class;
      State      : Network_State_Interface'Class;
      Local_Env  : Local_Environment'Class;
      Current    : Expression_Object_Interface'Class)
      return Real
   is
      No_Values : Array_Of_Values (1 .. 0);

      function V (X : Real) return Array_Of_Values
      is (1 => To_Expression_Value (X));

      function O (X : not null access Expression_Object_Interface'Class)
                  return Array_Of_Values
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
               declare
                  Var : constant String := Node.Variable_Name.all;
               begin
                  if Current.Has_Field (Var) then
                     return (1 => Current.Get_Field_Value (Var));
                  elsif Local_Env.Map.Contains (Var) then
                     return V (Local_Env.Map.Element (Var));
                  elsif State.Has_Field (Var) then
                     return (1 => State.Get_Field_Value (Var));
                  else
                     return O (State.Node (Var));
                  end if;
               end;
            when Constraint_Node =>
               return State.Evaluate_Constraint
                 (Class_Name       => Node.Constraint_Class.all,
                  Constraint_Name  => Node.Constraint_Name.all,
                  Constraint_Value =>
                    (if Node.Constraint_Value = null
                     then "" else Node.Constraint_Value.all));
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

   -------------------------
   -- Evaluate_Constraint --
   -------------------------

   overriding function Evaluate_Constraint
     (From             : Null_Network_State;
      Class_Name       : String;
      Constraint_Name  : String;
      Constraint_Value : String)
      return Array_Of_Values
   is
      pragma Unreferenced (From, Class_Name,
                           Constraint_Name, Constraint_Value);
   begin
      return X : Array_Of_Values (1 .. 0);
   end Evaluate_Constraint;

   ---------------------
   -- Get_Field_Value --
   ---------------------

   overriding function Get_Field_Value
     (Value : Null_Network_State;
      Name  : String)
      return Expression_Value
   is
      pragma Unreferenced (Value, Name);
   begin
      return To_Expression_Value (0.0);
   end Get_Field_Value;

   ---------------
   -- Get_Value --
   ---------------

   overriding function Get_Value
     (Value : Null_Network_State)
      return Expression_Value
   is
      pragma Unreferenced (Value);
   begin
      return To_Expression_Value (0.0);
   end Get_Value;

   ---------------
   -- Has_Field --
   ---------------

   overriding function Has_Field
     (Value : Null_Network_State;
      Name  : String)
      return Boolean
   is
      pragma Unreferenced (Value, Name);
   begin
      return False;
   end Has_Field;

   ----------
   -- Node --
   ----------

   overriding function Node
     (State : Null_Network_State;
      Name  : String)
      return Node_State_Access
   is
      pragma Unreferenced (State, Name);
   begin
      return null;
   end Node;

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

   ----------
   -- Show --
   ----------

   function Show
     (Expression : Expression_Type'Class)
      return String
   is
      function Show_Node (Node : Expression_Node) return String;

      ---------------
      -- Show_Node --
      ---------------

      function Show_Node (Node : Expression_Node) return String is
      begin
         if Node = null then
            return "()";
         end if;

         case Node.Node_Type is
            when Constant_Node =>
               return Concorde.Real_Images.Approximate_Image
                 (Node.Constant_Value);
            when Variable_Node =>
               return Node.Variable_Name.all;
            when Constraint_Node =>
               return "(" & Node.Constraint_Class.all
                 & " | " & Node.Constraint_Name.all
                 & (if Node.Constraint_Value /= null
                    then " => " & Node.Constraint_Value.all
                    else "")
                 & ")";
            when Field_Selector_Node =>
               return Show_Node (Node.Field_Container)
                 & "." & Node.Field_Name.all;
            when Primitive_Node =>
               if Node.Primitive = Negate then
                  return "-" & Show_Node (Node.Prim_Args.First_Element);
               elsif Node.Primitive = Multiply then
                  return "(" & Show_Node (Node.Prim_Args.First_Element)
                    & " * "
                    & Show_Node
                    (Expression_Node_Lists.Element
                       (Expression_Node_Lists.Next (Node.Prim_Args.First)))
                    & ")";
               elsif Node.Primitive in Add | Subtract then
                  return "(" & Show_Node (Node.Prim_Args.First_Element)
                    & (if Node.Primitive = Add then " + " else " - ")
                    & Show_Node
                    (Expression_Node_Lists.Element
                       (Expression_Node_Lists.Next (Node.Prim_Args.First)))
                    & ")";
               elsif Node.Primitive = Sum then
                  return Character'Val (16#CE#)
                    & Character'Val (16#A3#)
                    & "("
                    & Show_Node (Node.Prim_Args.First_Element)
                    & ")";
               else
                  return Node.Primitive'Image;
               end if;
         end case;
      end Show_Node;

   begin
      return Show_Node (Expression.Root);
   end Show;

end Concorde.Network.Expressions;
