package Concorde.Xi_UI.Model_Manager is

   function Model
     (For_Object : Concorde.Objects.Object_Type;
      Window     : Xi.Render_Window.Xi_Render_Window)
      return Xi_Model;

   procedure Load_Top_Model (Window : Xi.Render_Window.Xi_Render_Window);

end Concorde.Xi_UI.Model_Manager;
