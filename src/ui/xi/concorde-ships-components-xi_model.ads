with Xi.Entity;
with Xi.Node;

package Concorde.Ships.Components.Xi_Model is

   function Component_Entity
     (Component : not null access constant Root_Component_Type'Class)
      return Xi.Entity.Xi_Entity;

   procedure Scale_Component_Node
     (Component : not null access constant Root_Component_Type'Class;
      Node      : Xi.Node.Xi_Node);

end Concorde.Ships.Components.Xi_Model;
