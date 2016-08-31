with Xi.Assets;
with Xi.Color;
with Xi.Frame_Event;
with Xi.Main;
with Xi.Materials.Material;
with Xi.Shapes;

with Xi.Float_Arrays;
with Xi.Matrices;

with Concorde.Xi_UI.Key_Bindings;

package body Concorde.Xi_UI is

   Selector_Size : constant := 48;
   Selector_Boundary_Size : constant := 6;

   Local_Selector_Texture : Xi.Texture.Xi_Texture := null;
   Local_Selector_Entity  : Xi.Entity.Xi_Entity := null;

   type Model_Frame_Listener is
     new Xi.Frame_Event.Xi_Frame_Listener_Interface with
      record
         Model : Xi_Model;
      end record;

   overriding procedure Frame_Started
     (Listener : in out Model_Frame_Listener;
      Event    : Xi.Frame_Event.Xi_Frame_Event);

   --------------
   -- Activate --
   --------------

   procedure Activate (Model : in out Root_Xi_Model) is
   begin
      Model.Renderer.Set_Scene (Model.Scene);
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
   -- Initialize --
   ----------------

   procedure Initialize
     (Model    : not null access Root_Xi_Model'Class;
      Renderer : not null access
        Xi.Scene_Renderer.Xi_Scene_Renderer_Record'Class)
   is
   begin
      Model.Current_Renderer := Xi.Scene_Renderer.Xi_Scene_Renderer (Renderer);

      declare
         Listener : constant Xi.Frame_Event.Xi_Frame_Listener :=
                      new Model_Frame_Listener'(Model => Xi_Model (Model));
      begin
         Xi.Main.Add_Frame_Listener (Listener);
      end;
   end Initialize;

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

   --------------------
   -- On_Frame_Start --
   --------------------

   procedure On_Frame_Start
     (Model      : in out Root_Xi_Model;
      Time_Delta : Duration)
   is
      use type Xi.Scene.Xi_Scene;
      use type Concorde.Transitions.Transition_Type;
   begin

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

--        if Model.Active_Transition then
--           Model.Active_Transition.Update_Transition;
--           if Model.Active_Transition.Complete then
--
--           declare
--              use Ada.Calendar;
--              use Xi;
--              use Xi.Float_Arrays;
--              Now : constant Time := Clock;
--              Progress : constant Xi_Float :=
--                           Xi_Float (Now - Model.Start_Time)
--                           / Xi_Float (Model.Transition_Time);
--           begin
--              if Progress >= 1.0 then
--                 Model.Active_Transition := False;
--                 Model.Scene.Active_Camera.Set_Position
--                   (Model.Target_Position);
--                 Model.Scene.Active_Camera.Set_Orientation
--                   (Model.Target_Orientation);
--                 Model.Scene.Active_Camera.Set_Projection_Matrix
--                   (Model.Target_Projection);
--                 Root_Xi_Model'Class (Model).On_Transition_Complete;
--              else
--                 Model.Scene.Active_Camera.Set_Position
--                   (Model.Start_Position + Progress * Model.Position_Delta);
--                 Model.Scene.Active_Camera.Set_Orientation
--                   (Model.Start_Orientation
--                    + Progress * Model.Orientation_Delta);
--                 Model.Scene.Active_Camera.Set_Projection_Matrix
--                   (Model.Start_Projection
--                    + Progress * Model.Projection_Delta);
--              end if;
--           end;
--        end if;
   end On_Frame_Start;

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
         if False then
            Local_Selector_Entity.Set_Material
              (Xi.Assets.Material ("Xi.Blue"));
         else
            Local_Selector_Entity.Set_Material
              (Xi.Materials.Material.Xi_New_With_Texture
                 ("selector", Selector_Texture, Lighting => False));
         end if;
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

   ------------
   -- Window --
   ------------

--     function Window
--       (Model : Root_Xi_Model'Class)
--        return Xi.Render_Window.Xi_Render_Window
--     is
--     begin
--        return Model.Window;
--     end Window;

end Concorde.Xi_UI;
