with Xi.Scene_Renderer;
with Xi.Viewport;

with Concorde.Xi_UI;

with Concorde.Worlds;

package Concorde.Systems.Xi_Model is

--     procedure Transit_To_World
--       (World     : Concorde.Worlds.World_Type;
--        Model     : in out Concorde.Xi_UI.Root_Xi_Model'Class);

   function System_Model
     (System  : Star_System_Type;
      Target  : not null access
        Xi.Scene_Renderer.Xi_Scene_Renderer_Record'Class)
      return Concorde.Xi_UI.Xi_Model;

end Concorde.Systems.Xi_Model;
