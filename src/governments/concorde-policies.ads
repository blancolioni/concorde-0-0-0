with Accord.Network.Nodes;

package Accord.Policies is

   type Accord_Policy_Type is
     new Accord.Network.Nodes.Root_Accord_Node with private;

private

   type Accord_Policy_Type is
     new Accord.Network.Nodes.Root_Accord_Node with
      record
         Active : Boolean := False;
      end record;

   overriding function Is_Active (Policy : Accord_Policy_Type) return Boolean
   is (Policy.Active);

   overriding procedure Set_Initial_Value
     (Policy        : in out Accord_Policy_Type;
      Initial_Value : Non_Negative_Real);

end Accord.Policies;
