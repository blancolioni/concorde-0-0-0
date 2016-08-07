with Xi.Viewport;

with Concorde.Xi_UI;

with Concorde.Worlds;

package Concorde.Systems.Xi_Model is

   procedure Transit_To_World
     (World     : Concorde.Worlds.World_Type;
      Model     : in out Concorde.Xi_UI.Root_Xi_Model'Class);

end Concorde.Systems.Xi_Model;
