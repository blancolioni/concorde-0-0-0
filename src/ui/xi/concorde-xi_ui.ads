private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Calendar;

with Xi.Entity;
with Xi.Node;
with Xi.Render_Window;
with Xi.Scene_Renderer;
with Xi.Scene;
with Xi.Texture;

with Xtk.Div_Element;
with Xtk.Label;
with Xtk.Panel;
with Xtk.Text.View;

with Concorde.Objects;
with Concorde.Transitions;

with Concorde.Factions;

package Concorde.Xi_UI is

   type User_Command is
     (No_Command,
      Exit_Model,
      Move_Forward, Move_Backward,
      Move_Left, Move_Right,
      Move_Up, Move_Down,
      Zoom_In, Zoom_Out);

   type Root_Xi_Model is abstract tagged private;

   procedure Initialize
     (Model    : not null access Root_Xi_Model'Class;
      Faction  : Concorde.Factions.Faction_Type;
      Renderer : not null access
        Xi.Scene_Renderer.Xi_Scene_Renderer_Record'Class);

   procedure Activate (Model : in out Root_Xi_Model);

   function Base_Movement
     (Model : Root_Xi_Model)
      return Xi.Xi_Float
   is (10.0);

   function Base_Rotation
     (Model : Root_Xi_Model)
      return Xi.Xi_Float
   is (5.0);

   procedure On_User_Command
     (Model      : in out Root_Xi_Model;
      Time_Delta : Duration;
      Command    : User_Command);

   function Top_Panel
     (Model : Root_Xi_Model)
      return Xtk.Panel.Xtk_Panel
   is (null);

   procedure Transit_To_Object
     (Model         : in out Root_Xi_Model;
      Target_Object : not null access constant
        Concorde.Objects.Root_Object_Type'Class)
   is abstract;

   procedure On_Frame_Start
     (Model      : in out Root_Xi_Model;
      Time_Delta : Duration);

   procedure Set_Status
     (Model   : in out Root_Xi_Model;
      Message : String);

   type Xi_Model is access all Root_Xi_Model'Class;

   procedure Add_Transition
     (Model      : in out Root_Xi_Model'Class;
      Transition : Concorde.Transitions.Transition_Type);

   procedure Move_In
     (Model : in out Root_Xi_Model;
      Scale : Xi.Xi_Signed_Unit_Float);

   procedure Move_Horizontal
     (Model : in out Root_Xi_Model;
      Scale : Xi.Xi_Signed_Unit_Float);

   procedure Move_Vertical
     (Model : in out Root_Xi_Model;
      Scale : Xi.Xi_Signed_Unit_Float);

   function Scene
     (Model : Root_Xi_Model'Class)
      return Xi.Scene.Xi_Scene;

   procedure Set_Scene
     (Model : in out Root_Xi_Model'Class;
      Scene : Xi.Scene.Xi_Scene);

   function Renderer
     (Model : Root_Xi_Model'Class)
      return Xi.Scene_Renderer.Xi_Scene_Renderer;

   procedure Set_Renderer
     (Model : in out Root_Xi_Model'Class;
      Target : not null access
        Xi.Scene_Renderer.Xi_Scene_Renderer_Record'Class);

   function Selector_Texture return Xi.Texture.Xi_Texture;
   function Selector_Entity return Xi.Entity.Xi_Entity;

   type Select_Handler_Interface is interface;

   procedure On_Select
     (Handler : Select_Handler_Interface)
   is abstract;

   type Select_Handler is access all Select_Handler_Interface'Class;

   function Null_Selector return Select_Handler;

   type Node_Select_Handler is abstract new
     Select_Handler_Interface with private;

   overriding procedure On_Select
     (Handler : Node_Select_Handler);

   procedure On_Node_Selected
     (Handler : Node_Select_Handler;
      Node    : Xi.Node.Xi_Node)
   is abstract;

   function Selector_With_Text
     (Parent_Node : Xi.Node.Xi_Node;
      Text        : String;
      X, Y, Z     : Xi.Xi_Float;
      On_Select   : Select_Handler)
      return Xi.Node.Xi_Node;

   procedure Load_UI
     (Window : Xi.Render_Window.Xi_Render_Window;
      Path   : String);

   function Outliner_Div return Xtk.Div_Element.Xtk_Div_Element;
   function Main_Log_View return Xtk.Text.View.Xtk_Text_View;

private

   type Node_Select_Handler is abstract new
     Select_Handler_Interface with
      record
         Node : Xi.Node.Xi_Node;
      end record;

   package Active_Transition_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Concorde.Transitions.Transition_Type,
        Concorde.Transitions."=");

   type Root_Xi_Model is abstract tagged
      record
         Active             : Boolean := False;
         Current_Scene      : Xi.Scene.Xi_Scene;
         Current_Renderer   : Xi.Scene_Renderer.Xi_Scene_Renderer;
         Active_Transitions : Active_Transition_Lists.List;
         Current_Transition : Concorde.Transitions.Transition_Type;
         Frame_Count        : Natural;
         Start_Interval     : Ada.Calendar.Time;
         Elapsed_Time       : Duration;
         Faction            : Concorde.Factions.Faction_Type;
         Status             : Xtk.Panel.Xtk_Panel;
         Status_Label       : Xtk.Label.Xtk_Label;
         FPS_Label          : Xtk.Label.Xtk_Label;
         Clock_Label        : Xtk.Label.Xtk_Label;
         Cash_Label         : Xtk.Label.Xtk_Label;
         Faction_Name_Label : Xtk.Label.Xtk_Label;
      end record;

--   function Main_UI return Xtk.Builder.Xtk_Builder;

   type Model_Signal_Handler is
     abstract new Concorde.Objects.Object_Handler_Interface with
      record
         Model : Xi_Model;
      end record;

end Concorde.Xi_UI;
