with Ada.Strings.Fixed;

with Concorde.Network.Expressions;
with Concorde.Real_Images;

package body Concorde.Network.State is

   -------------------------
   -- Calculate_New_Value --
   -------------------------

   overriding procedure Calculate_New_Value
     (Node_State    : in out Root_Node_State_Type;
      Network_State : Network_State_Interface'Class)
   is
      New_Value : Real := 0.0;

      procedure Eval
        (Input_Node    : Concorde.Network.Nodes.Node_Type;
         Input_Inertia : Duration;
         Expression    : Concorde.Network.Expressions.Expression_Type);

      ----------
      -- Eval --
      ----------

      procedure Eval
        (Input_Node    : Concorde.Network.Nodes.Node_Type;
         Input_Inertia : Duration;
         Expression    : Concorde.Network.Expressions.Expression_Type)
      is
         Identity : constant String := Input_Node.Identifier;
         State_Node : constant Node_State_Access :=
                        Network_State.Node (Identity);
      begin
         New_Value := New_Value
           + Expression.Evaluate (Network_State, Identity,
                                  State_Node.Current_Inertial_Value
                                    (Input_Inertia));
      end Eval;

   begin
      Node_State.Node.Scan_Inputs
        (Eval'Access);
      Node_State.New_Value := Unit_Clamp (New_Value);
   end Calculate_New_Value;

   --------------------------
   -- Current_Actual_Value --
   --------------------------

   overriding function Current_Actual_Value
     (Node_State : Root_Node_State_Type)
      return Real
   is
   begin
      return Root_Node_State_Type'Class (Node_State).Current_Value
        * Root_Node_State_Type'Class (Node_State).Base_Value;
   end Current_Actual_Value;

   ------------------------
   -- Current_Base_Value --
   ------------------------

   overriding function Current_Base_Value
     (Node_State : Root_Node_State_Type)
      return Real
   is
   begin
      return Node_State.Base_Value;
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
     (Node_State : Root_Node_State_Type)
      return Unit_Real
   is
   begin
      return Node_State.Current_Value;
   end Current_Value;

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
     (Node       : Concorde.Network.Nodes.Node_Type)
      return Node_State_Access
   is
   begin
      return new Root_Node_State_Type'
        (Node          => Node,
         others        => <>);
   end New_Internal_Node_State;

   -----------------------
   -- Set_Initial_Value --
   -----------------------

   overriding procedure Set_Initial_Value
     (Node_State    : in out Root_Node_State_Type;
      Value         : Real)
   is
   begin
      Node_State.Base_Value := Value;
   end Set_Initial_Value;

   -------------------
   -- Set_New_Value --
   -------------------

   overriding procedure Set_New_Value
     (Node_State : in out Root_Node_State_Type)
   is
   begin
      Node_State.Current_Value := Node_State.New_Value;
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
