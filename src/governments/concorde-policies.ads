private with Ada.Containers.Vectors;
private with Memor.Database;

with Concorde.Objects;

with Concorde.Network.Nodes;

package Concorde.Policies is

   type Root_Policy_Type is
     new Concorde.Objects.Root_Localised_Object_Type with private;

   type Policy_Type is access constant Root_Policy_Type'Class;

   procedure Scan_Policies
     (Process : not null access
        procedure (Policy : Policy_Type));

   function Policy_Node
     (Policy : Root_Policy_Type'Class)
      return Concorde.Network.Nodes.Node_Type;

private

   type Root_Policy_Type is
     new Concorde.Objects.Root_Localised_Object_Type with
      record
         Active     : Boolean := False;
         Node       : Concorde.Network.Nodes.Node_Type;
      end record;

   overriding function Object_Database
     (Item : Root_Policy_Type)
      return Memor.Memor_Database;

   package Db is
     new Memor.Database
       ("policy", Root_Policy_Type, Policy_Type);

   overriding function Object_Database
     (Item : Root_Policy_Type)
      return Memor.Memor_Database
   is (Db.Get_Database);

   package Policy_Vectors is
     new Ada.Containers.Vectors (Positive, Policy_Type);

   Vector : Policy_Vectors.Vector;

end Concorde.Policies;
