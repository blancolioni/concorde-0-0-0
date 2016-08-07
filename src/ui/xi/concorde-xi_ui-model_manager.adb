with Concorde.Xi_UI.Galaxies;

package body Concorde.Xi_UI.Model_Manager is

   Top_Model : Xi_Model := null;
   pragma Unreferenced (Top_Model);

   --------------------
   -- Load_Top_Model --
   --------------------

   procedure Load_Top_Model (Window : Xi.Render_Window.Xi_Render_Window) is
   begin
      Top_Model := Model (null, Window);
   end Load_Top_Model;

   -----------
   -- Model --
   -----------

   function Model
     (For_Object : Concorde.Objects.Object_Type;
      Window     : Xi.Render_Window.Xi_Render_Window)
      return Xi_Model
   is
      use type Concorde.Objects.Object_Type;
   begin
      if For_Object = null then
         return Concorde.Xi_UI.Galaxies.Galaxy_Model (Window);
      else
         return null;
      end if;
   end Model;

end Concorde.Xi_UI.Model_Manager;
