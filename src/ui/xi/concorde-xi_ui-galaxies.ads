with Concorde.Systems;

package Concorde.Xi_UI.Galaxies is

   function Galaxy_Model
     (Renderer : not null access
        Xi.Scene_Renderer.Xi_Scene_Renderer_Record'Class)
      return Xi_Model;

end Concorde.Xi_UI.Galaxies;
