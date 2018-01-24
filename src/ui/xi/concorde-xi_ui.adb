with WL.Money;

with Css;

with Xi.Assets;
with Xi.Color;
with Xi.Font;
with Xi.Frame_Event;
with Xi.Keyboard;
with Xi.Main;
with Xi.Label;
with Xi.Materials.Material;
with Xi.Mouse;
with Xi.Shapes;

with Xi.Float_Arrays;
with Xi.Matrices;

with Xtk.Builder;

with Xtk.Button;
with Xtk.Page;

with Concorde.Xi_UI.Key_Bindings;
with Concorde.Xi_UI.Outliner;
--  with Concorde.Xi_UI.Portraits;

with Concorde.Calendar;
with Concorde.Updates;

with Concorde.Options;

package body Concorde.Xi_UI is

   Selector_Size : constant := 48;
   Selector_Boundary_Size : constant := 6;

   Local_Selector_Texture : Xi.Texture.Xi_Texture := null;
   Local_Selector_Entity  : Xi.Entity.Xi_Entity := null;

   Local_Outliner_Div  : Xtk.Div_Element.Xtk_Div_Element;

   Update_Multiplier   : Non_Negative_Real := 24.0;

   Local_Main_UI : Xtk.Builder.Xtk_Builder;

   type Model_Frame_Listener is
     new Xi.Frame_Event.Xi_Frame_Listener_Interface with
      record
         Model        : Xi_Model;
      end record;

   overriding procedure Frame_Started
     (Listener : in out Model_Frame_Listener;
      Event    : Xi.Frame_Event.Xi_Frame_Event);

   type Null_Selector_Record is new Select_Handler_Interface
   with null record;

   overriding procedure On_Select
     (Handler : Null_Selector_Record)
   is null;

   Local_Null_Selector_Record : aliased Null_Selector_Record;

   type Selector_Node is
     new Xi.Node.Xi_Node_Record with
      record
         On_Select : Select_Handler;
      end record;

   type Selector_Node_Access is access all Selector_Node'Class;

   procedure On_Selector_Clicked
     (Selector : Xi.Node.Xi_Node);

   type Update_Speed_Button is
     new Xtk.Button.Xtk_Button_Record with
      record
         Target_Speed : Xi.Xi_Float;
      end record;

--     overriding procedure On_Click
--       (Button  : in out Update_Speed_Button;
--        Mouse   : in Xi.Mouse.Mouse_Button;
--        Control : in Xi.Keyboard.Control_Mask);

   function New_Speed_Button
     (Tooltip : String;
      Speed   : Xi.Xi_Non_Negative_Float;
      Id      : String)
      return Xtk.Button.Xtk_Button with Unreferenced;

   --------------
   -- Activate --
   --------------

   procedure Activate (Model : in out Root_Xi_Model) is
   begin
      Model.Renderer.Set_Scene (Model.Scene);
      Model.Elapsed_Time := 0.0;
      Model.Frame_Count := 0;
      Model.Active := True;
   end Activate;

   --------------------
   -- Add_Transition --
   --------------------

   procedure Add_Transition
     (Model              : in out Root_Xi_Model'Class;
      Transition         : Concorde.Transitions.Transition_Type)
   is
   begin
      Model.Active_Transitions.Append (Transition);
      --    Model.Active_Transition := True;
      --    Model.Start_Position := Model.Scene.Active_Camera.Position;
      --    Model.Start_Orientation := Model.Scene.Active_Camera.Orientation;
      --  Model.Start_Projection:= Model.Scene.Active_Camera.Projection_Matrix;
      --        Model.Target_Position := Target_Position & 1.0;
      --        Model.Target_Orientation := Target_Orientation;
      --        Model.Target_Projection := Target_Projection;
      --  Model.Position_Delta := Model.Target_Position - Model.Start_Position;
      --        Model.Orientation_Delta :=
      --          Model.Target_Orientation - Model.Start_Orientation;
      --        Model.Projection_Delta :=
      --          Model.Target_Projection - Model.Start_Projection;
      --        Model.Transition_Time := Transition_Time;
      --        Model.Start_Time := Ada.Calendar.Clock;
   end Add_Transition;

   -------------------
   -- Frame_Started --
   -------------------

   overriding procedure Frame_Started
     (Listener : in out Model_Frame_Listener;
      Event    : Xi.Frame_Event.Xi_Frame_Event)
   is
   begin
      Listener.Model.On_Frame_Start (Event.Time_Since_Last_Event);
   end Frame_Started;

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget
     (Name : String)
      return Xtk.Widget.Xtk_Widget
   is
   begin
      return Local_Main_UI.Get (Name);
   end Get_Widget;

   ------------------
   -- Hide_Overlay --
   ------------------

   procedure Hide_Overlay
     (Model   : in out Root_Xi_Model;
      Overlay : Overlay_Type)
   is
      use Overlay_Lists;
      Position : Cursor := No_Element;
   begin
      for It in Model.Info_Panels.Iterate loop
         if Element (It) = Overlay then
            Position := It;
            exit;
         end if;
      end loop;
      if Has_Element (Position) then
         Element (Position).On_Hide;
         Element (Position).Panel.Hide;
         Model.Info_Panels.Delete (Position);
      end if;
   end Hide_Overlay;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Model    : not null access Root_Xi_Model'Class;
      Faction  : Concorde.Factions.Faction_Type;
      Renderer : not null access
        Xi.Scene_Renderer.Xi_Scene_Renderer_Record'Class)
   is
   begin
      Model.Faction := Faction;
      Model.Current_Renderer := Xi.Scene_Renderer.Xi_Scene_Renderer (Renderer);

      Model.FPS_Label :=
        Xtk.Label.Xtk_Label (Local_Main_UI.Get ("fps"));
      Model.Clock_Label :=
        Xtk.Label.Xtk_Label (Local_Main_UI.Get ("clock"));
      Model.Cash_Label :=
        Xtk.Label.Xtk_Label (Local_Main_UI.Get ("cash"));
      Model.Faction_Name_Label :=
        Xtk.Label.Xtk_Label (Local_Main_UI.Get ("faction-name"));

      Model.Faction_Name_Label.Set_Label (Faction.Name);

      Model.Show_Clock_Time := Concorde.Options.Show_Clock_Time;
      Model.Log_Ship_Movement := Concorde.Options.Log_Ship_Movement;

      declare
         Listener : constant Xi.Frame_Event.Xi_Frame_Listener :=
                      new Model_Frame_Listener'
                        (Model        => Xi_Model (Model));
      begin
         Xi.Main.Add_Frame_Listener (Listener);
      end;

      Update_Multiplier :=
        Non_Negative_Real (Concorde.Options.Update_Speed);

   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Overlay    : in out Root_Overlay_Type'Class;
      Element_Id : String)
   is
   begin
      Overlay.Panel :=
        Xtk.Panel.Xtk_Panel
          (Local_Main_UI.Get (Element_Id));
   end Initialize;

   -------------
   -- Load_UI --
   -------------

   procedure Load_UI
     (Window : Xi.Render_Window.Xi_Render_Window;
      Path   : String)
   is
   begin
--        Concorde.Xi_UI.Portraits.Register;

      declare
         Builder : constant Xtk.Builder.Xtk_Builder :=
                     Xtk.Builder.Xtk_New_From_File (Path);
         Page    : constant Xtk.Page.Xtk_Page :=
                     Builder.Get_Page;
      begin
         Page.Set_Viewport (Window.Full_Viewport);
         Page.Show_All;
         Window.Add_Top_Level (Page);

         Local_Main_UI := Builder;
      end;
   end Load_UI;

   -------------------
   -- Main_Log_View --
   -------------------

   function Main_Log_View return Xtk.Text.View.Xtk_Text_View is
   begin
      return Xtk.Text.View.Xtk_Text_View
        (Local_Main_UI.Get ("log"));
   end Main_Log_View;

   ---------------------
   -- Move_Horizontal --
   ---------------------

   procedure Move_Horizontal
     (Model : in out Root_Xi_Model;
      Scale : Xi.Xi_Signed_Unit_Float)
   is
   begin
      Model.Scene.Active_Camera.Translate (Scale, 0.0, 0.0);
   end Move_Horizontal;

   -------------
   -- Move_In --
   -------------

   procedure Move_In
     (Model : in out Root_Xi_Model;
      Scale : Xi.Xi_Signed_Unit_Float)
   is
   begin
      Model.Scene.Active_Camera.Translate (0.0, 0.0, Scale);
   end Move_In;

   -------------------
   -- Move_Vertical --
   -------------------

   procedure Move_Vertical
     (Model : in out Root_Xi_Model;
      Scale : Xi.Xi_Signed_Unit_Float)
   is
   begin
      Model.Scene.Active_Camera.Translate (0.0, Scale, 0.0);
   end Move_Vertical;

   ----------------------
   -- New_Speed_Button --
   ----------------------

   function New_Speed_Button
     (Tooltip : String;
      Speed   : Xi.Xi_Non_Negative_Float;
      Id      : String)
      return Xtk.Button.Xtk_Button
   is
      pragma Unreferenced (Tooltip);
      Result : Update_Speed_Button;
   begin
      Result.Create ("", Id);
      Result.Target_Speed := Speed;
      return new Update_Speed_Button'(Result);
   end New_Speed_Button;

   -------------------
   -- Null_Selector --
   -------------------

   function Null_Selector return Select_Handler is
   begin
      return Local_Null_Selector_Record'Access;
   end Null_Selector;

   --------------
   -- On_Click --
   --------------

--     overriding procedure On_Click
--       (Button  : in out Update_Speed_Button;
--        Mouse   : in Xi.Mouse.Mouse_Button;
--        Control : in Xi.Keyboard.Control_Mask)
--     is
--        pragma Unreferenced (Mouse);
--        pragma Unreferenced (Control);
--     begin
--        Concorde.Updates.Set_Time_Acceleration (Button.Target_Speed);
--     end On_Click;

   --------------------
   -- On_Frame_Start --
   --------------------

   procedure On_Frame_Start
     (Model      : in out Root_Xi_Model;
      Time_Delta : Duration)
   is
      use type Xi.Scene.Xi_Scene;
      use type Xtk.Label.Xtk_Label;
      use type Concorde.Transitions.Transition_Type;
   begin

      if Xi.Keyboard.Key_Down (Xi.Keyboard.Key_Esc) then
         Xi.Main.Leave_Main_Loop;
         return;
      end if;

      Concorde.Updates.Advance
        (Duration (Update_Multiplier) * Time_Delta);

      Model.Cash_Label.Set_Label
        (WL.Money.Show (Model.Faction.Cash));

      if Model.Active then
         if Model.Frame_Count = 0 then
            Model.Start_Interval := Ada.Calendar.Clock;
         end if;

         Model.Frame_Count := Model.Frame_Count + 1;

         if Model.Frame_Count = 300 then
            declare
               use Ada.Calendar;
               D : constant Duration :=
                     Ada.Calendar.Clock - Model.Start_Interval;
            begin
               Model.FPS_Label.Set_Label
                 (Natural'Image
                    (Natural
                         (Real (Model.Frame_Count)
                          / Real (D))));
               Model.Frame_Count := 0;
            end;
         end if;

         if Model.Clock_Label /= null then
            Model.Clock_Label.Set_Label
              (Concorde.Calendar.Image
                 (Concorde.Calendar.Clock, Model.Show_Clock_Time));
         end if;

      end if;

      if Model.Current_Transition /= null then
         Model.Current_Transition.Update;
         if Model.Current_Transition.Complete then
            Model.Current_Transition := null;
         end if;
      elsif not Model.Active_Transitions.Is_Empty then
         Model.Current_Transition := Model.Active_Transitions.First_Element;
         Model.Active_Transitions.Delete_First;
         if Model.Current_Transition.Scene = null then
            Model.Current_Transition.Set_Scene (Model.Scene);
         elsif Model.Current_Transition.Scene /= Model.Scene then
            Model.Current_Scene := Model.Current_Transition.Scene;
            Model.Renderer.Set_Scene (Model.Scene);
         end if;
         Model.Current_Transition.Start;
      else
         declare
            use type Xi.Xi_Float;
            use Concorde.Xi_UI.Key_Bindings;
            Active : constant Array_Of_Key_Bindings :=
                       Current_Active_Bindings;
         begin
            for Command of Active loop
               Root_Xi_Model'Class (Model).On_User_Command
                 (Time_Delta, Command);
            end loop;
         end;
      end if;

      Concorde.Xi_UI.Outliner.Render;

   end On_Frame_Start;

   ---------------
   -- On_Select --
   ---------------

   overriding procedure On_Select
     (Handler : Node_Select_Handler)
   is
   begin
      Node_Select_Handler'Class (Handler).On_Node_Selected (Handler.Node);
   end On_Select;

   -------------------------
   -- On_Selector_Clicked --
   -------------------------

   procedure On_Selector_Clicked
     (Selector : Xi.Node.Xi_Node)
   is
   begin
      Selector_Node_Access (Selector).On_Select.On_Select;
   end On_Selector_Clicked;

   ---------------------
   -- On_User_Command --
   ---------------------

   procedure On_User_Command
     (Model      : in out Root_Xi_Model;
      Time_Delta : Duration;
      Command    : User_Command)
   is
      use type Xi.Xi_Float;
      use Xi.Matrices, Xi.Float_Arrays;
      Class : Root_Xi_Model'Class renames Root_Xi_Model'Class (Model);
      Translation : Vector_3 := (0.0, 0.0, 0.0);
      Yaw         : Xi.Xi_Float := 0.0;
      Pitch       : Xi.Xi_Float := 0.0;
   begin
      case Command is
         when No_Command =>
            null;
         when Exit_Model =>
            Xi.Main.Leave_Main_Loop;
         when Move_Forward =>
            Translation (3) := -Class.Base_Movement;
         when Move_Backward =>
            Translation (3) := Class.Base_Movement;
         when Move_Right =>
            Yaw := -Class.Base_Rotation;
         when Move_Left =>
            Yaw := Class.Base_Rotation;
         when Move_Up =>
            Pitch := -Class.Base_Rotation;
         when Move_Down =>
            Pitch := Class.Base_Rotation;
         when Zoom_In =>
            null;
         when Zoom_Out =>
            null;
      end case;

      if abs Translation /= 0.0 then
         Model.Scene.Active_Camera.Translate
           (Model.Scene.Active_Camera.Orientation
            * Translation
            * Xi.Xi_Float (Time_Delta));
      end if;

      if Pitch /= 0.0 then
         Model.Scene.Active_Camera.Rotate
           (Pitch * Xi.Xi_Float (Time_Delta),
            1.0, 0.0, 0.0);
      end if;

      if Yaw /= 0.0 then
         Model.Scene.Active_Camera.Rotate
           (Yaw * Xi.Xi_Float (Time_Delta),
            0.0, 1.0, 0.0);
      end if;

   end On_User_Command;

   ------------------
   -- Outliner_Div --
   ------------------

   function Outliner_Div return Xtk.Div_Element.Xtk_Div_Element is
      use type Xtk.Div_Element.Xtk_Div_Element;
   begin
      if Local_Outliner_Div = null then
         Local_Outliner_Div :=
           Xtk.Div_Element.Xtk_Div_Element
             (Local_Main_UI.Get ("outline-table"));
      end if;
      return Local_Outliner_Div;
   end Outliner_Div;

   --------------
   -- Renderer --
   --------------

   function Renderer
     (Model : Root_Xi_Model'Class)
      return Xi.Scene_Renderer.Xi_Scene_Renderer
   is
   begin
      return Model.Current_Renderer;
   end Renderer;

   -----------
   -- Scene --
   -----------

   function Scene
     (Model : Root_Xi_Model'Class)
      return Xi.Scene.Xi_Scene
   is
   begin
      return Model.Current_Scene;
   end Scene;

   ---------------------
   -- Selector_Entity --
   ---------------------

   function Selector_Entity return Xi.Entity.Xi_Entity is
      use type Xi.Entity.Xi_Entity;
   begin
      if Local_Selector_Entity = null then
         Local_Selector_Entity :=
           Xi.Shapes.Square (Xi.Xi_Float (1.0));
         Local_Selector_Entity.Set_Material
           (Xi.Materials.Material.Xi_New_With_Texture
              ("selector", Selector_Texture, Lighting => False));
         Local_Selector_Entity.Material.Technique (1).Pass (1).Alpha_Discard
           (Xi.Materials.Less_Than, 0.5);
      end if;
      return Local_Selector_Entity;
   end Selector_Entity;

   ----------------------
   -- Selector_Texture --
   ----------------------

   function Selector_Texture return Xi.Texture.Xi_Texture is
      use type Xi.Texture.Xi_Texture;
   begin
      if Local_Selector_Texture = null then
         if False then
            Local_Selector_Texture := Xi.Assets.Texture ("default");
         else
            declare
               Data : Xi.Color.Xi_Color_2D_Array
                 (1 .. Selector_Size, 1 .. Selector_Size);
            begin
               for Y in Data'Range (2) loop
                  for X in Data'Range (1) loop
                     if X <= Selector_Boundary_Size
                       or else X > Selector_Size - Selector_Boundary_Size
                       or else Y <= Selector_Boundary_Size
                       or else Y > Selector_Size - Selector_Boundary_Size
                     then
                        Data (X, Y) := (0.2, 0.8, 0.4, 1.0);
                     else
                        Data (X, Y) := (0.0, 0.0, 0.0, 0.0);
                     end if;
                  end loop;
               end loop;
               Local_Selector_Texture :=
                 Xi.Texture.Create_From_Data ("selector-texture", Data);
            end;
         end if;
      end if;
      return Local_Selector_Texture;
   end Selector_Texture;

   ------------------------
   -- Selector_With_Text --
   ------------------------

   function Selector_With_Text
     (Parent_Node : Xi.Node.Xi_Node;
      Text        : String;
      X, Y, Z     : Xi.Xi_Float;
      On_Select   : Select_Handler)
      return Xi.Node.Xi_Node
   is
      use Xi;
      Node : constant Xi.Node.Xi_Node :=
               Parent_Node.Create_Child ("selector");
      Label  : constant Xi.Label.Xi_Label :=
                 Xi.Label.Create_Label
                   ("selector-label",
                    Text,
                    Xi.Font.Get_Font ("Segoe UI", 10.0));
      Target_Node : constant Selector_Node_Access :=
                      new Selector_Node;
      Text_Node   : constant Xi.Node.Xi_Node :=
                      Node.Create_Child ("selector-text");
   begin
      Target_Node.Initialize ("selector-target");
      Node.Append_Child (Target_Node);
      Target_Node.On_Select := On_Select;
      Target_Node.Set_Entity (Selector_Entity);
      Target_Node.Set_Position (X / 2.0, Y / 2.0, Z / 2.0);
      Target_Node.Set_Billboard (True);
      Target_Node.Fixed_Pixel_Size (32.0, 32.0);
      Text_Node.Set_Entity (Label);
      Text_Node.Set_Position (X / 2.0, Y / 2.0, Z / 2.0);
      Text_Node.Set_Billboard (True);
      Text_Node.Fixed_Pixel_Size (Label.Width, Label.Height, 32.0, 16.0);

      Target_Node.Add_Click_Handler (On_Selector_Clicked'Access);

      return Node;
   end Selector_With_Text;

   ------------------
   -- Set_Renderer --
   ------------------

   procedure Set_Renderer
     (Model  : in out Root_Xi_Model'Class;
      Target : not null access
        Xi.Scene_Renderer.Xi_Scene_Renderer_Record'Class)
   is
   begin
      Model.Current_Renderer :=
        Xi.Scene_Renderer.Xi_Scene_Renderer (Target);
   end Set_Renderer;

   ---------------
   -- Set_Scene --
   ---------------

   procedure Set_Scene
     (Model : in out Root_Xi_Model'Class;
      Scene : Xi.Scene.Xi_Scene)
   is
   begin
      Model.Current_Scene := Scene;
   end Set_Scene;

   ----------------
   -- Set_Status --
   ----------------

   procedure Set_Status
     (Model   : in out Root_Xi_Model;
      Message : String)
   is
   begin
      if False then
         Model.Status_Label.Set_Label (Message);
      end if;
   end Set_Status;

   ------------------
   -- Show_Overlay --
   ------------------

   procedure Show_Overlay
     (Model : in out Root_Xi_Model;
      Overlay : Overlay_Type;
      X, Y  : Natural)
   is
      Found : Boolean := False;
   begin
      Overlay.Panel.Set_Style
        ("top", Css.Pixels (Y));
      Overlay.Panel.Set_Style
        ("left", Css.Pixels (X));

      for Item of Model.Info_Panels loop
         if Item = Overlay then
            Found := True;
            exit;
         end if;
      end loop;

      if not Found then
         Model.Info_Panels.Append (Overlay);
      end if;

   end Show_Overlay;

end Concorde.Xi_UI;
