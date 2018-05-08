with Xi.Node;

package Concorde.Ships.Modules.Xi_Model is

   function Create_Module_Node
     (Module : not null access constant Root_Module_Type'Class;
      Parent : Xi.Node.Xi_Node)
      return Xi.Node.Xi_Node;

end Concorde.Ships.Modules.Xi_Model;
