with Xi.Node;

with Concorde.Xi_UI;

package Concorde.Worlds.Xi_Model is

   procedure Load_World
     (World       : World_Type;
      Parent_Node : Xi.Node.Xi_Node);

   procedure Transit_To_World
     (World     : Concorde.Worlds.World_Type;
      Model     : in out Concorde.Xi_UI.Root_Xi_Model'Class);

end Concorde.Worlds.Xi_Model;
