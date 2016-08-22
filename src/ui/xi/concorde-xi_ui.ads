private with Ada.Containers.Doubly_Linked_Lists;

with Xi.Entity;
with Xi.Render_Window;
with Xi.Scene;
with Xi.Texture;

with Xtk.Panel;

with Concorde.Objects;
with Concorde.Transitions;

package Concorde.Xi_UI is

   type Root_Xi_Model is abstract tagged private;

   procedure On_Wheel_Up (Model : in out Root_Xi_Model);
   procedure On_Wheel_Down (Model : in out Root_Xi_Model);

   function Top_Panel
     (Model : Root_Xi_Model)
      return Xtk.Panel.Xtk_Panel
      is abstract;

   procedure Transit_To_Object
     (Model         : in out Root_Xi_Model;
      Target_Object : not null access constant
        Concorde.Objects.Root_Object_Type'Class)
   is abstract;

   procedure On_Frame_Start
     (Model : in out Root_Xi_Model);

   function Window
     (Model : Root_Xi_Model'Class)
      return Xi.Render_Window.Xi_Render_Window;

   type Xi_Model is access all Root_Xi_Model'Class;

   procedure Add_Transition
     (Model              : in out Root_Xi_Model'Class;
      Transition         : Concorde.Transitions.Transition_Type);

--     procedure On_Transition_Update
--       (Model    : in out Root_Xi_Model;
--        Progress : Xi.Xi_Unit_Float);
--
--     procedure On_Transition_Complete
--       (Model : in out Root_Xi_Model)
--     is null;

   procedure Move_In
     (Model : in out Root_Xi_Model;
      Scale : Xi.Xi_Signed_Unit_Float);

   procedure Move_Horizontal
     (Model : in out Root_Xi_Model;
      Scale : Xi.Xi_Signed_Unit_Float);

   procedure Move_Vertical
     (Model : in out Root_Xi_Model;
      Scale : Xi.Xi_Signed_Unit_Float);

   function Selector_Texture return Xi.Texture.Xi_Texture;
   function Selector_Entity return Xi.Entity.Xi_Entity;

private

   package Active_Transition_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Concorde.Transitions.Transition_Type,
        Concorde.Transitions."=");

   type Root_Xi_Model is abstract tagged
      record
         Scene              : Xi.Scene.Xi_Scene;
         Window             : Xi.Render_Window.Xi_Render_Window;
         Active_Transitions : Active_Transition_Lists.List;
         Current_Transition : Concorde.Transitions.Transition_Type;
      end record;

end Concorde.Xi_UI;
