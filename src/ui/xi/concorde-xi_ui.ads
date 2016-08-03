with Ada.Calendar;

with Xi.Matrices;
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
     (For_Object : Concorde.Objects.Object_Type)
      return Xi_Model;

   function Scene
     (Model : Root_Xi_Model'Class)
      return Xi.Scene.Xi_Scene;

   procedure Start_Transition
     (Model              : in out Root_Xi_Model'Class;
      Target_Position    : Xi.Matrices.Vector_3;
      Target_Orientation : Xi.Matrices.Matrix_3;
      Transition_Time    : Duration);

private

   type Root_Xi_Model is abstract tagged
      record
         Scene              : Xi.Scene.Xi_Scene;
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
