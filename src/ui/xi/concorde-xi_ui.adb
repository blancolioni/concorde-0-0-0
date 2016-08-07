with Xi.Main;

with Concorde.Xi_UI.Key_Bindings;

package body Concorde.Xi_UI is

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
     (Model : in out Root_Xi_Model)
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
         if Model.Current_Transition.Scene /= Model.Scene then
            Model.Scene := Model.Current_Transition.Scene;
            Model.Window.Set_Scene (Model.Scene);
         end if;
         Model.Current_Transition.Start;
      else
         declare
            use type Xi.Xi_Float;
            use Concorde.Xi_UI.Key_Bindings;
            Active : constant Array_Of_Key_Bindings :=
                       Current_Active_Bindings;
         begin
            for Binding of Active loop
               case Binding is
                  when No_Binding =>
                     null;
                  when Exit_Model =>
                     Xi.Main.Leave_Main_Loop;
                  when Move_Forward =>
                     Root_Xi_Model'Class (Model).Move_In (0.1);
                  when Move_Backward =>
                     Root_Xi_Model'Class (Model).Move_In (-0.1);
                  when Move_Right =>
                     Root_Xi_Model'Class (Model).Move_Horizontal (0.1);
                  when Move_Left =>
                     Root_Xi_Model'Class (Model).Move_Horizontal (-0.1);
                  when Move_Up =>
                     Root_Xi_Model'Class (Model).Move_Vertical (0.1);
                  when Move_Down =>
                     Root_Xi_Model'Class (Model).Move_Vertical (-0.1);
               end case;
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

   -------------------
   -- On_Wheel_Down --
   -------------------

   procedure On_Wheel_Down (Model : in out Root_Xi_Model) is
      use type Xi.Xi_Float;
   begin
      Model.Scene.Active_Camera.Translate
        (0.0, 0.0, 0.1);
   end On_Wheel_Down;

   -----------------
   -- On_Wheel_Up --
   -----------------

   procedure On_Wheel_Up (Model : in out Root_Xi_Model) is
      use type Xi.Xi_Float;
   begin
      Model.Scene.Active_Camera.Translate
        (0.0, 0.0, -0.1);
   end On_Wheel_Up;

   ------------
   -- Window --
   ------------

   function Window
     (Model : Root_Xi_Model'Class)
      return Xi.Render_Window.Xi_Render_Window
   is
   begin
      return Model.Window;
   end Window;

end Concorde.Xi_UI;
