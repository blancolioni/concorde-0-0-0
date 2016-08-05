with Ada.Calendar;

with Xi.Matrices;
with Xi.Render_Window;
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

   procedure Transit_To_Object
     (Model         : in out Root_Xi_Model;
      Target_Object : not null access constant
        Concorde.Objects.Root_Object_Type'Class)
   is abstract;

   procedure On_Frame_Start
     (Model : in out Root_Xi_Model);

   type Xi_Model is access all Root_Xi_Model'Class;

   function Model
     (For_Object : Concorde.Objects.Object_Type;
      Window     : Xi.Render_Window.Xi_Render_Window)
      return Xi_Model;

   procedure Start_Transition
     (Model              : in out Root_Xi_Model'Class;
      Target_Position    : Xi.Matrices.Vector_3;
      Target_Orientation : Xi.Matrices.Matrix_3;
      Transition_Time    : Duration);

   procedure On_Transition_Complete
     (Model : in out Root_Xi_Model)
   is null;

private

   type Root_Xi_Model is abstract tagged
      record
         Scene              : Xi.Scene.Xi_Scene;
         Window             : Xi.Render_Window.Xi_Render_Window;
         Active_Transition  : Boolean := False;
         Target_Position    : Xi.Matrices.Vector_4;
         Target_Orientation : Xi.Matrices.Matrix_3;
         Start_Position     : Xi.Matrices.Vector_4;
         Start_Orientation  : Xi.Matrices.Matrix_3;
         Position_Delta     : Xi.Matrices.Vector_4;
         Orientation_Delta  : Xi.Matrices.Matrix_3;
         Start_Time         : Ada.Calendar.Time;
         Transition_Time    : Duration;
      end record;

end Concorde.Xi_UI;
