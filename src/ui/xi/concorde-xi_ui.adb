with Xi.Float_Arrays;

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

   --------------------
   -- On_Frame_Start --
   --------------------

   procedure On_Frame_Start
     (Model : in out Root_Xi_Model)
   is
   begin
      if Model.Active_Transition then
         declare
            use Ada.Calendar;
            use Xi;
            use Xi.Float_Arrays;
            Now : constant Time := Clock;
            Progress : constant Xi_Float :=
                         Xi_Float (Now - Model.Start_Time)
                         / Xi_Float (Model.Transition_Time);
         begin
            if Progress >= 1.0 then
               Model.Active_Transition := False;
               Model.Scene.Active_Camera.Set_Position
                 (Model.Target_Position);
               Model.Scene.Active_Camera.Set_Orientation
                 (Model.Target_Orientation);
            else
               Model.Scene.Active_Camera.Set_Position
                 (Model.Start_Position + Progress * Model.Position_Delta);
               Model.Scene.Active_Camera.Set_Orientation
                 (Model.Start_Orientation
                  + Progress * Model.Orientation_Delta);
            end if;
         end;
      end if;
   end On_Frame_Start;

   -------------------
   -- On_Wheel_Down --
   -------------------

   procedure On_Wheel_Down (Model : in out Root_Xi_Model) is
      use type Xi.Xi_Float;
   begin
      Model.Scene.Active_Camera.Translate
        (0.0, 0.0, 0.01);
   end On_Wheel_Down;

   -----------------
   -- On_Wheel_Up --
   -----------------

   procedure On_Wheel_Up (Model : in out Root_Xi_Model) is
      use type Xi.Xi_Float;
   begin
      Model.Scene.Active_Camera.Translate
        (0.0, 0.0, -0.01);
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

   ----------------------
   -- Start_Transition --
   ----------------------

   procedure Start_Transition
     (Model              : in out Root_Xi_Model'Class;
      Target_Position    : Xi.Matrices.Vector_3;
      Target_Orientation : Xi.Matrices.Matrix_3;
      Transition_Time    : Duration)
   is
      use Xi.Float_Arrays;
   begin
      Model.Active_Transition := True;
      Model.Start_Position := Model.Scene.Active_Camera.Position;
      Model.Start_Orientation := Model.Scene.Active_Camera.Orientation;
      Model.Target_Position := Target_Position & 1.0;
      Model.Target_Orientation := Target_Orientation;
      Model.Position_Delta := Model.Target_Position - Model.Start_Position;
      Model.Orientation_Delta :=
        Model.Target_Orientation - Model.Start_Orientation;
      Model.Transition_Time := Transition_Time;
      Model.Start_Time := Ada.Calendar.Clock;
   end Start_Transition;

end Concorde.Xi_UI;
