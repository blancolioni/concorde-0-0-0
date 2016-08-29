with Xi.Node;
with Xi.Scene;

with Concorde.Xi_UI;

package Concorde.Ships.Xi_Model is

   procedure Transit_To_Ship
     (Ship   : Ship_Type;
      Model  : in out Concorde.Xi_UI.Root_Xi_Model'Class);

   procedure Create_Ship_Node
     (Ship    : Ship_Type;
      Scene   : Xi.Scene.Xi_Scene;
      Primary : Xi.Node.Xi_Node);

end Concorde.Ships.Xi_Model;
