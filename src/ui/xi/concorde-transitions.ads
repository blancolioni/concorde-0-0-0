private with Ada.Calendar;

with Xi.Matrices;
with Xi.Scene;

package Concorde.Transitions is

   type Root_Transition_Type is tagged private;

   procedure Create
     (Transition         : in out Root_Transition_Type;
      Scene              : Xi.Scene.Xi_Scene;
      Target_Position    : Xi.Matrices.Vector_3;
      Target_Orientation : Xi.Matrices.Matrix_3;
      Target_Projection  : Xi.Matrices.Matrix_4;
      Acceleration       : Xi.Xi_Non_Negative_Float;
      Max_Velocity       : Xi.Xi_Non_Negative_Float);

   procedure Create
     (Transition         : in out Root_Transition_Type'Class;
      Scene              : Xi.Scene.Xi_Scene;
      Target_Position    : Xi.Matrices.Vector_3;
      Target_Orientation : Xi.Matrices.Matrix_3;
      Target_Projection  : Xi.Matrices.Matrix_4;
      Transition_Time    : Duration);

   procedure Create
     (Transition         : in out Root_Transition_Type'Class;
      Scene              : Xi.Scene.Xi_Scene;
      Target_Position    : Xi.Matrices.Vector_3;
      Target_Orientation : Xi.Matrices.Matrix_3;
      Transition_Time    : Duration);

   procedure Create
     (Transition         : in out Root_Transition_Type;
      Scene              : Xi.Scene.Xi_Scene;
      Target_Position    : Xi.Matrices.Vector_3;
      Target_Orientation : Xi.Matrices.Matrix_3;
      Acceleration       : Xi.Xi_Non_Negative_Float;
      Max_Velocity       : Xi.Xi_Non_Negative_Float);

   procedure Create
     (Transition         : in out Root_Transition_Type'Class;
      Target_Position    : Xi.Matrices.Vector_3;
      Target_Orientation : Xi.Matrices.Matrix_3;
      Acceleration       : Xi.Xi_Non_Negative_Float;
      Max_Velocity       : Xi.Xi_Non_Negative_Float);

   procedure Create
     (Transition         : in out Root_Transition_Type'Class;
      Target_Position    : Xi.Matrices.Vector_3;
      Target_Orientation : Xi.Matrices.Matrix_3;
      Transition_Time    : Duration);

   procedure Create
     (Transition         : in out Root_Transition_Type'Class;
      Target_Position    : Xi.Matrices.Vector_3;
      Acceleration       : Xi.Xi_Non_Negative_Float;
      Max_Velocity       : Xi.Xi_Non_Negative_Float);

   procedure Scene_Transition
     (Transition : in out Root_Transition_Type;
      New_Scene  : Xi.Scene.Xi_Scene);

   procedure Start
     (Transition : in out Root_Transition_Type);

   procedure Update
     (Transition : in out Root_Transition_Type);

   function Complete
     (Transition : Root_Transition_Type)
      return Boolean;

   function Current_Position
     (Transition : Root_Transition_Type)
      return Xi.Matrices.Vector_3;

   function Current_Orientation
     (Transition : Root_Transition_Type)
      return Xi.Matrices.Matrix_3;

   function Current_Projection
     (Transition : Root_Transition_Type)
      return Xi.Matrices.Matrix_4;

   function Scene
     (Transition : Root_Transition_Type)
     return Xi.Scene.Xi_Scene;

   procedure Set_Scene
     (Transition : in out Root_Transition_Type;
      Scene      : Xi.Scene.Xi_Scene);

   type Transition_Type is access all Root_Transition_Type'Class;

private

   type Root_Transition_Type is tagged
      record
         Transit_Position     : Boolean := False;
         Transit_Orientation  : Boolean := False;
         Transit_Projection   : Boolean := False;
         Scene                : Xi.Scene.Xi_Scene;
         Target_Position      : Xi.Matrices.Vector_4;
         Target_Orientation   : Xi.Matrices.Matrix_3;
         Target_Projection    : Xi.Matrices.Matrix_4;
         Start_Position       : Xi.Matrices.Vector_4;
         Start_Orientation    : Xi.Matrices.Matrix_3;
         Start_Projection     : Xi.Matrices.Matrix_4;
         Position_Delta       : Xi.Matrices.Vector_4;
         Orientation_Delta    : Xi.Matrices.Matrix_3;
         Projection_Delta     : Xi.Matrices.Matrix_4;
         Current_Position     : Xi.Matrices.Vector_4;
         Current_Orientation  : Xi.Matrices.Matrix_3;
         Current_Projection   : Xi.Matrices.Matrix_4;
         Start_Time           : Ada.Calendar.Time;
         Last_Update          : Ada.Calendar.Time;
         Acceleration         : Xi.Xi_Non_Negative_Float;
         Max_Velocity         : Xi.Xi_Non_Negative_Float;
         Speed                : Xi.Xi_Non_Negative_Float;
         Vector               : Xi.Matrices.Vector_4;
         Distance             : Xi.Xi_Non_Negative_Float;
         Accel_Coefficient    : Xi.Xi_Non_Negative_Float;
         Coast_Coefficient    : Xi.Xi_Non_Negative_Float;
         Accelerate_Time      : Duration;
         Coast_Time           : Duration;
         Transition_Time      : Duration;
         Coast_Start_At       : Xi.Xi_Unit_Float;
         Decel_Start_At       : Xi.Xi_Unit_Float;
         Coast_Start_Progress : Xi.Xi_Unit_Float;
         Decel_Start_Progress : Xi.Xi_Unit_Float;
         Completed            : Boolean := False;
      end record;

end Concorde.Transitions;
