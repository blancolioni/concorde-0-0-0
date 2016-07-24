with Xi.Scene;
with Xtk.Panel;

with Concorde.Objects;

package Concorde.Xi_UI is

   type Root_Xi_Model is abstract tagged private;

   procedure On_Wheel_Up (Model : in out Root_Xi_Model);
   procedure On_Wheel_Down (Model : in out Root_Xi_Model);

   function Top_Panel
     (Model : Root_Xi_Model)
      return Xtk.Panel.Xtk_Panel
      is abstract;

   type Xi_Model is access all Root_Xi_Model'Class;

   function Model
     (For_Object : Concorde.Objects.Object_Type)
      return Xi_Model;

   function Scene
     (Model : Root_Xi_Model'Class)
      return Xi.Scene.Xi_Scene;

private

   type Root_Xi_Model is abstract tagged
      record
         Scene : Xi.Scene.Xi_Scene;
      end record;

end Concorde.Xi_UI;
