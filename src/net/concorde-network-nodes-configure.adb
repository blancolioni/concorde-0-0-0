with Ada.Text_IO;

package body Concorde.Network.Nodes.Configure is

   ----------------
   -- Add_Output --
   ----------------

   procedure Add_Output
     (From       : in out Root_Node_Type'Class;
      To         : not null access Root_Node_Type'Class;
      Expression : Concorde.Expressions.Expression_Type;
      Inertia    : Duration)
   is
   begin
      From.Outputs.Append
        (Node_Output'
           (Target     => Accord_Node_Access (To),
            Expression => Expression,
            Inertia    => Inertia));
   end Add_Output;

   --------------------
   -- Initial_Values --
   --------------------

   procedure Initial_Values
     (Network : Node_Map'Class;
      Get_Value : not null access
        function (Name : String) return Real)
   is
   begin
      for Node of Network.Map loop
         declare
            Value : constant Real :=
                      Get_Value (Node.Identity);
         begin
            if Value >= 0.0 then
               Node.Set_Initial_Value
                 (Get_Value (Node.Identity));
               Ada.Text_IO.Put_Line
                 (Node.Identity & " = " & Node.Show_Value);
            else
               Ada.Text_IO.Put_Line
                 (Node.Identity & ": inactive");
            end if;
         end;
      end loop;
   end Initial_Values;

end Concorde.Network.Nodes.Configure;
