pragma Ada_2012;
package body Accord.Policies is

   -----------------------
   -- Set_Initial_Value --
   -----------------------

   overriding procedure Set_Initial_Value
     (Policy        : in out Accord_Policy_Type;
      Initial_Value : Non_Negative_Real)
   is
   begin
      Accord.Network.Nodes.Root_Accord_Node (Policy)
        .Set_Initial_Value (Initial_Value);
      Policy.Active := True;
   end Set_Initial_Value;

end Accord.Policies;
