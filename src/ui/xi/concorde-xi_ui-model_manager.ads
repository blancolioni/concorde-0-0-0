with Xi.Scene_Renderer;

package Concorde.Xi_UI.Model_Manager is

   function Model
     (For_Object : access constant
        Concorde.Objects.Root_Object_Type'Class;
      Renderer   : not null access
        Xi.Scene_Renderer.Xi_Scene_Renderer_Record'Class)
      return Xi_Model;

   procedure Load_Top_Model
     (Renderer   : not null access
        Xi.Scene_Renderer.Xi_Scene_Renderer_Record'Class);

end Concorde.Xi_UI.Model_Manager;
