with Xi.Scene_Renderer;
with Xi.Viewport;

with Concorde.Xi_UI;

with Concorde.Factions;
with Concorde.Worlds;

package Concorde.Systems.Xi_Model is

--     procedure Transit_To_World
--       (World     : Concorde.Worlds.World_Type;
--        Model     : in out Concorde.Xi_UI.Root_Xi_Model'Class);

   type System_Model_View is
     (Accurate, Schematic);

   function System_Model
     (System  : Star_System_Type;
      Faction : Concorde.Factions.Faction_Type;
      View    : System_Model_View;
      Target  : not null access
        Xi.Scene_Renderer.Xi_Scene_Renderer_Record'Class)
      return Concorde.Xi_UI.Xi_Model;

end Concorde.Systems.Xi_Model;
