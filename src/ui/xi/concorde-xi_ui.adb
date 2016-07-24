with Concorde.Xi_UI.Galaxies;

package body Concorde.Xi_UI is

   -----------
   -- Model --
   -----------

   function Model
     (For_Object : Concorde.Objects.Object_Type)
      return Xi_Model
   is
      use type Concorde.Objects.Object_Type;
   begin
      if For_Object = null then
         return Concorde.Xi_UI.Galaxies.Galaxy_Model;
      else
         return null;
      end if;
   end Model;

   -------------------
   -- On_Wheel_Down --
   -------------------

   procedure On_Wheel_Down (Model : in out Root_Xi_Model) is
      use type Xi.Xi_Float;
   begin
      Model.Scene.Active_Camera.Translate
        (0.0, 0.0, Model.Scene.Active_Camera.Position_3 (3) * 0.01);
   end On_Wheel_Down;

   -----------------
   -- On_Wheel_Up --
   -----------------

   procedure On_Wheel_Up (Model : in out Root_Xi_Model) is
      use type Xi.Xi_Float;
   begin
      Model.Scene.Active_Camera.Translate
        (0.0, 0.0, -Model.Scene.Active_Camera.Position_3 (3) * 0.01);
   end On_Wheel_Up;

   -----------
   -- Scene --
   -----------

   function Scene
     (Model : Root_Xi_Model'Class)
      return Xi.Scene.Xi_Scene
   is
   begin
      return Model.Scene;
   end Scene;

end Concorde.Xi_UI;
