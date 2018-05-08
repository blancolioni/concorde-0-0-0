with Concorde.Ships.Components.Xi_Model;

package body Concorde.Ships.Modules.Xi_Model is

   ------------------------
   -- Create_Module_Node --
   ------------------------

   function Create_Module_Node
     (Module : not null access constant Root_Module_Type'Class;
      Parent : Xi.Node.Xi_Node)
      return Xi.Node.Xi_Node
   is
   begin
      return Node : constant Xi.Node.Xi_Node :=
        Parent.Create_Child (Module.Name)
      do
         Node.Set_Orientation_4 (Module.Orientation);
         Node.Set_Position (Module.Position);
         Node.Set_Entity
           (Concorde.Ships.Components.Xi_Model.Component_Entity
              (Module.Component));
         Concorde.Ships.Components.Xi_Model.Scale_Component_Node
           (Module.Component, Node);
      end return;
   end Create_Module_Node;

end Concorde.Ships.Modules.Xi_Model;
