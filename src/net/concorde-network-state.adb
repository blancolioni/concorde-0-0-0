with Ada.Strings.Fixed;

with Concorde.Real_Images;

with Concorde.Network.Expressions;

package body Concorde.Network.State is

   ----------------
   -- Add_Effect --
   ----------------

   overriding procedure Add_Effect
     (Node_State : in out Root_Node_State_Type;
      Effect     : Signed_Unit_Real)
   is
   begin
      Node_State.New_Value := Node_State.New_Value + Effect;
      Node_State.Changed := True;
   end Add_Effect;

   --------------------------
   -- Current_Actual_Value --
   --------------------------

   overriding function Current_Actual_Value
     (Node_State : Root_Node_State_Type)
      return Real
   is
   begin
      return Root_Node_State_Type'Class (Node_State).Current_Value;
   end Current_Actual_Value;

   ------------------------
   -- Current_Base_Value --
   ------------------------

   overriding function Current_Base_Value
     (Node_State : Root_Node_State_Type)
      return Real
   is
      pragma Unreferenced (Node_State);
   begin
      return 0.0;
   end Current_Base_Value;

   ----------------------------
   -- Current_Inertial_Value --
   ----------------------------

   overriding function Current_Inertial_Value
     (Node    : Root_Node_State_Type;
      Inertia : Duration)
      return Real
   is
      use Concorde.Calendar;
      Today     : constant Time := Clock;
      Start     : constant Time := Today - Inertia;
      Current   : Time := Today;
      Remaining : Duration := Inertia;
      Total     : Non_Negative_Real := 0.0;
   begin
      if Inertia = 0.0 or else Node.History.Is_Empty then
         return Node.Current_Value;
      end if;

      for History of reverse Node.History loop
         declare
            D : constant Duration :=
                  Current - Max (History.Date, Start);
         begin
            Total := Total + History.Value * Non_Negative_Real (D);
            Current := Current - D;
            Remaining := Remaining - D;
         end;
      end loop;

      Total := Total
        + Node.History.First_Element.Value * Non_Negative_Real (Remaining);

      return Total / Non_Negative_Real (Inertia);

   end Current_Inertial_Value;

   -------------------
   -- Current_Value --
   -------------------

   overriding function Current_Value
     (Node_State : Root_Node_State_Type) return Signed_Unit_Real
   is
   begin
      return Node_State.Current_Bias;
   end Current_Value;

   ---------------------
   -- Get_Field_Value --
   ---------------------

   overriding function Get_Field_Value
     (Node_State : Root_Node_State_Type;
      Name       : String)
      return Expression_Value
   is
      Node_Class : Root_Node_State_Type'Class renames
                     Root_Node_State_Type'Class (Node_State);
      Result : constant Real :=
                 (if Name = "current-value"
                  then Node_Class.Current_Value
                  elsif Name = "current-actual"
                  then Node_Class.Current_Actual_Value
                  elsif Name = "current-base"
                  then Node_Class.Current_Base_Value
                  elsif Node_State.Node.Has_Field (Name)
                  then (if Node_State.Fields.Contains (Name)
                    then Node_State.Fields.Element (Name)
                      else 0.0)
                  else raise Constraint_Error with
                    "no such field '" & Name & "' for node "
                  & Node_State.Identifier);
   begin
      return To_Expression_Value (Result);
   end Get_Field_Value;

   ---------------
   -- Get_Value --
   ---------------

   overriding function Get_Value
     (Node_State : Root_Node_State_Type)
      return Expression_Value
   is
   begin
      return To_Expression_Value (Node_State.Current_Value);
   end Get_Value;

   ---------------
   -- Has_Field --
   ---------------

   overriding function Has_Field
     (Node_State : Root_Node_State_Type;
      Name       : String)
      return Boolean
   is
   begin
      return Name = "current-value"
        or else Name = "current-actual"
        or else Name = "current-base"
        or else Node_State.Node.Has_Field (Name);
   end Has_Field;

   ----------------------
   -- Initialize_State --
   ----------------------

   procedure Initialize_State
     (State : in out Root_Node_State_Type'Class;
      Node  : not null access constant
        Concorde.Network.Nodes.Root_Node_Type'Class;
      Init  : Real)
   is
   begin
      State.Node := Concorde.Network.Nodes.Node_Type (Node);
      State.Set_Initial_Value (Init);
   end Initialize_State;

   ---------------
   -- Is_Active --
   ---------------

   overriding function Is_Active
     (Node_State : Root_Node_State_Type)
      return Boolean
   is
   begin
      return Node_State.Active;
   end Is_Active;

   -----------------------------
   -- New_Internal_Node_State --
   -----------------------------

   function New_Internal_Node_State
     (Node       : not null access constant
        Concorde.Network.Nodes.Root_Node_Type'Class)
      return Node_State_Access
   is
   begin
      return new Root_Node_State_Type'
        (Node          => Concorde.Network.Nodes.Node_Type (Node),
         others        => <>);
   end New_Internal_Node_State;

   ------------------
   -- Send_Effects --
   ------------------

   overriding procedure Send_Effects
     (Node_State    : in out Root_Node_State_Type;
      Network_State : Network_State_Interface'Class)
   is

      procedure Send_Effect
        (Target_Node  : Node_State_Access;
         Effect_Delay : Duration;
         Effect       : Concorde.Network.Expressions.Expression_Type);

      -----------------
      -- Send_Effect --
      -----------------

      procedure Send_Effect
        (Target_Node  : Node_State_Access;
         Effect_Delay : Duration;
         Effect       : Concorde.Network.Expressions.Expression_Type)
      is
      begin
         Target_Node.Add_Effect
           (Concorde.Network.Expressions.Evaluate
              (Expression     => Effect,
               Env            => Network_State,
               Argument_Name  => "x",
               Argument_Value =>
                 Node_State.Current_Inertial_Value (Effect_Delay)));
      end Send_Effect;

   begin
      Node_State.Node.Scan_Effects (Network_State, Send_Effect'Access);
   end Send_Effects;

   -----------------------
   -- Set_Initial_Value --
   -----------------------

   overriding procedure Set_Initial_Value
     (Node_State    : in out Root_Node_State_Type;
      Value         : Real)
   is
   begin
      Node_State.Current_Bias := Value;
      Node_State.Changed := False;
   end Set_Initial_Value;

   -------------------
   -- Set_New_Value --
   -------------------

   overriding procedure Set_New_Value
     (Node_State    : in out Root_Node_State_Type;
      Network_State : Network_State_Interface'Class)
   is

      Update_Env : Concorde.Network.Expressions.Local_Environment;

      procedure Update_Field
        (Field_Name : String;
         Definition : Concorde.Network.Expressions.Expression_Type);

      ------------------
      -- Update_Field --
      ------------------

      procedure Update_Field
        (Field_Name : String;
         Definition : Concorde.Network.Expressions.Expression_Type)
      is
         X : constant Real :=
               Definition.Evaluate
                 (Network_State, Node_State);
      begin
         if not Node_State.Fields.Contains (Field_Name) then
            Node_State.Fields.Insert (Field_Name, X);
         else
            Node_State.Fields (Field_Name) := X;
         end if;
      end Update_Field;

   begin

      if Node_State.Changed then
         Node_State.Current_Bias :=
           Signed_Unit_Clamp (Node_State.New_Value);
         Node_State.New_Value := 0.0;
         Node_State.Changed := False;
      end if;

      declare
         State_Class : Root_Node_State_Type'Class renames
                         Root_Node_State_Type'Class (Node_State);
      begin
         Update_Env.Add ("current-actual",
                         State_Class.Current_Actual_Value);
         Update_Env.Add ("current-value",
                         State_Class.Current_Value);
         Update_Env.Add ("current-base",
                         State_Class.Current_Base_Value);

         Node_State.Node.Scan_Fields (Update_Field'Access);
      end;

   end Set_New_Value;

   ----------------
   -- Show_Value --
   ----------------

   overriding function Show_Value
     (Node_State : Root_Node_State_Type)
      return String
   is
      Current : constant Real :=
                  Root_Node_State_Type'Class (Node_State).Current_Value;
   begin
      if Current > 0.0 and then Current < 1.0 then
         return Ada.Strings.Fixed.Trim
           (Natural'Image (Natural (Current * 100.0)) & "%",
            Ada.Strings.Left);
      elsif Current > -1.0 and then Current < 0.0 then
         return Integer'Image (Integer (Current * 100.0)) & "%";
      else
         return Concorde.Real_Images.Approximate_Image (Current);
      end if;
   end Show_Value;

end Concorde.Network.State;
