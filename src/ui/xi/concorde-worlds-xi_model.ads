with Xi.Node;
with Xi.Scene_Renderer;

with Concorde.Xi_UI;

package Concorde.Worlds.Xi_Model is

   function World_Model
     (World  : World_Type;
      Target     : not null access
        Xi.Scene_Renderer.Xi_Scene_Renderer_Record'Class)
      return Concorde.Xi_UI.Xi_Model;

   procedure Load_World
     (World       : World_Type;
      Parent_Node : Xi.Node.Xi_Node);

   procedure Transit_To_World
     (World           : Concorde.Worlds.World_Type;
      Model           : in out Concorde.Xi_UI.Root_Xi_Model'Class);

end Concorde.Worlds.Xi_Model;
