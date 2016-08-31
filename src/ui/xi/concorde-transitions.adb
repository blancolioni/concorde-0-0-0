with Xi.Elementary_Functions;
with Xi.Float_Arrays;
with Xi.Logging;

package body Concorde.Transitions is

   --------------
   -- Complete --
   --------------

   function Complete
     (Transition : Root_Transition_Type)
      return Boolean
   is
   begin
      return Transition.Completed;
   end Complete;

   ------------
   -- Create --
   ------------

   procedure Create
     (Transition         : in out Root_Transition_Type;
      Scene              : Xi.Scene.Xi_Scene;
      Target_Position    : Xi.Matrices.Vector_3;
      Target_Orientation : Xi.Matrices.Matrix_3;
      Target_Projection  : Xi.Matrices.Matrix_4;
      Acceleration       : Xi.Xi_Non_Negative_Float;
      Max_Velocity       : Xi.Xi_Non_Negative_Float)
   is
      use Xi.Float_Arrays;
   begin
      Transition.Transit_Position := True;
      Transition.Transit_Orientation := True;
      Transition.Transit_Projection := True;

      Transition.Scene := Scene;

      Transition.Acceleration := Acceleration;
      Transition.Max_Velocity := Max_Velocity;

      Transition.Target_Position := Target_Position & 1.0;
      Transition.Target_Orientation := Target_Orientation;
      Transition.Target_Projection := Target_Projection;

   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Transition         : in out Root_Transition_Type'Class;
      Scene              : Xi.Scene.Xi_Scene;
      Target_Position    : Xi.Matrices.Vector_3;
      Target_Orientation : Xi.Matrices.Matrix_3;
      Target_Projection  : Xi.Matrices.Matrix_4;
      Transition_Time    : Duration)
   is
      use Xi.Float_Arrays;
   begin
      Transition.Transit_Position := True;
      Transition.Transit_Orientation := True;
      Transition.Transit_Projection := True;

      Transition.Scene := Scene;

      Transition.Acceleration := 0.0;
      Transition.Max_Velocity := 0.0;
      Transition.Transition_Time := Transition_Time;

      Transition.Target_Position := Target_Position & 1.0;
      Transition.Target_Orientation := Target_Orientation;
      Transition.Target_Projection := Target_Projection;

   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Transition         : in out Root_Transition_Type'Class;
      Scene              : Xi.Scene.Xi_Scene;
      Target_Position    : Xi.Matrices.Vector_3;
      Target_Orientation : Xi.Matrices.Matrix_3;
      Transition_Time    : Duration)
   is
   begin
      Transition.Create (Scene, Target_Position, Target_Orientation,
                         Xi.Float_Arrays.Unit_Matrix (4),
                         Transition_Time);
      Transition.Transit_Position := True;
      Transition.Transit_Orientation := True;
      Transition.Transit_Projection := False;
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Transition         : in out Root_Transition_Type;
      Scene              : Xi.Scene.Xi_Scene;
      Target_Position    : Xi.Matrices.Vector_3;
      Target_Orientation : Xi.Matrices.Matrix_3;
      Acceleration       : Xi.Xi_Non_Negative_Float;
      Max_Velocity       : Xi.Xi_Non_Negative_Float)
   is
   begin
      Transition.Create (Scene, Target_Position, Target_Orientation,
                         Xi.Float_Arrays.Unit_Matrix (4),
                         Acceleration, Max_Velocity);
      Transition.Transit_Position := True;
      Transition.Transit_Orientation := True;
      Transition.Transit_Projection := False;
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Transition         : in out Root_Transition_Type'Class;
      Target_Position    : Xi.Matrices.Vector_3;
      Target_Orientation : Xi.Matrices.Matrix_3;
      Acceleration       : Xi.Xi_Non_Negative_Float;
      Max_Velocity       : Xi.Xi_Non_Negative_Float)
   is
   begin
      Transition.Create (null, Target_Position, Target_Orientation,
                         Xi.Float_Arrays.Unit_Matrix (4),
                         Acceleration, Max_Velocity);
      Transition.Transit_Position := True;
      Transition.Transit_Orientation := True;
      Transition.Transit_Projection := False;
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Transition         : in out Root_Transition_Type'Class;
      Target_Position    : Xi.Matrices.Vector_3;
      Acceleration       : Xi.Xi_Non_Negative_Float;
      Max_Velocity       : Xi.Xi_Non_Negative_Float)
   is
   begin
      Transition.Create (null, Target_Position,
                         Xi.Float_Arrays.Unit_Matrix (3),
                         Xi.Float_Arrays.Unit_Matrix (4),
                         Acceleration, Max_Velocity);
      Transition.Transit_Position := True;
      Transition.Transit_Orientation := False;
      Transition.Transit_Projection := False;
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Transition         : in out Root_Transition_Type'Class;
      Target_Position    : Xi.Matrices.Vector_3;
      Target_Orientation : Xi.Matrices.Matrix_3;
      Transition_Time    : Duration)
   is
   begin
      Transition.Create (null, Target_Position, Target_Orientation,
                         Transition_Time);
   end Create;

   -------------------------
   -- Current_Orientation --
   -------------------------

   function Current_Orientation
     (Transition : Root_Transition_Type)
      return Xi.Matrices.Matrix_3
   is
   begin
      return Transition.Current_Orientation;
   end Current_Orientation;

   ----------------------
   -- Current_Position --
   ----------------------

   function Current_Position
     (Transition : Root_Transition_Type)
      return Xi.Matrices.Vector_3
   is
   begin
      return Transition.Current_Position (1 .. 3);
   end Current_Position;

   ------------------------
   -- Current_Projection --
   ------------------------

   function Current_Projection
     (Transition : Root_Transition_Type)
      return Xi.Matrices.Matrix_4
   is
   begin
      return Transition.Current_Projection;
   end Current_Projection;

   -----------
   -- Scene --
   -----------

   function Scene
     (Transition : Root_Transition_Type)
      return Xi.Scene.Xi_Scene
   is
   begin
      return Transition.Scene;
   end Scene;

   ----------------------
   -- Scene_Transition --
   ----------------------

   procedure Scene_Transition
     (Transition : in out Root_Transition_Type;
      New_Scene  : Xi.Scene.Xi_Scene)
   is
   begin
      Transition.Create
        (New_Scene,
         New_Scene.Active_Camera.Position_3,
         New_Scene.Active_Camera.Orientation,
         0.1);
   end Scene_Transition;

   ---------------
   -- Set_Scene --
   ---------------

   procedure Set_Scene
     (Transition : in out Root_Transition_Type;
      Scene      : Xi.Scene.Xi_Scene)
   is
   begin
      Transition.Scene := Scene;
   end Set_Scene;

   -----------
   -- Start --
   -----------

   procedure Start
     (Transition : in out Root_Transition_Type)
   is
      use Xi, Xi.Float_Arrays;
      use Xi.Elementary_Functions;
      Scene : Xi.Scene.Xi_Scene renames Transition.Scene;
   begin

      Transition.Start_Position := Scene.Active_Camera.Position;
      Transition.Start_Orientation := Scene.Active_Camera.Orientation;
      Transition.Start_Projection := Scene.Active_Camera.Projection_Matrix;

      Xi.Logging.Put ("transition ");
      Xi.Logging.Put (Xi_Float (Transition.Transition_Time));
      Xi.Logging.Put ("s: ");
      if Transition.Transit_Position then
         Xi.Logging.Put ("position: ");
         Xi.Logging.Put (Transition.Start_Position);
         Xi.Logging.Put (" -> ");
         Xi.Logging.Put (Transition.Target_Position);
      end if;
      if Transition.Transit_Orientation then
         Xi.Logging.Put ("; orientation");
      end if;
      if Transition.Transit_Projection then
         Xi.Logging.Put ("; projection");
      end if;

      Xi.Logging.New_Line;

      Transition.Position_Delta :=
        Transition.Target_Position - Transition.Start_Position;
      Transition.Orientation_Delta :=
        Transition.Target_Orientation - Transition.Start_Orientation;
      Transition.Projection_Delta :=
        Transition.Target_Projection - Transition.Start_Projection;

      if Transition.Acceleration > 0.0 then
         declare
            Max_Velocity : constant Xi_Float := Transition.Max_Velocity;
            Acceleration : constant Xi_Float := Transition.Acceleration;
            T            : constant Xi_Non_Negative_Float :=
                             Max_Velocity / Acceleration;
            S            : constant Xi_Non_Negative_Float :=
                             0.5 * Acceleration * T * T;
            D            : Xi_Non_Negative_Float := 0.0;
            X1           : Xi.Matrices.Vector_3 renames
                             Transition.Start_Position;
            X2           : Xi.Matrices.Vector_3 renames
                             Transition.Target_Position;
         begin
            for I in X1'Range loop
               D := D + (X1 (I) - X2 (I)) ** 2;
            end loop;
            D := Sqrt (D);
            Transition.Distance := D;

            if S <= D / 2.0 then
               Transition.Accelerate_Time := Duration (T);
               Transition.Coast_Time :=
                 Duration ((D - 2.0 * S) / Max_Velocity);
            else
               Transition.Accelerate_Time :=
                 Duration (T * D / S);
               Transition.Coast_Time := 0.0;
            end if;

            Transition.Transition_Time :=
              Transition.Accelerate_Time * 2.0
                + Transition.Coast_Time;
            Transition.Accel_Coefficient :=
              (S / D) / ((T / Xi_Float (Transition.Transition_Time)) ** 2);

            Transition.Coast_Start_At :=
              Xi_Float (Transition.Accelerate_Time)
              / Xi_Float (Transition.Transition_Time);
            Transition.Decel_Start_At := 1.0 - Transition.Coast_Start_At;
            if Transition.Coast_Time > 0.0 then
               Transition.Coast_Start_Progress :=
                 Transition.Accel_Coefficient
                   * Transition.Coast_Start_At ** 2;
               Transition.Decel_Start_Progress :=
                 1.0 - Transition.Coast_Start_Progress;
               Transition.Coast_Coefficient :=
                 (Transition.Decel_Start_Progress
                  - Transition.Coast_Start_Progress)
                 / (Transition.Decel_Start_At - Transition.Coast_Start_At);
            else
               Transition.Coast_Start_Progress := 0.5;
               Transition.Decel_Start_Progress := 0.5;
               Transition.Coast_Coefficient := 1.0;
            end if;
         end;
      end if;

      Transition.Vector :=
        Xi.Matrices.Normalise (Transition.Position_Delta);
      Transition.Speed := 0.0;

      Transition.Current_Position := Transition.Start_Position;
      Transition.Current_Orientation := Transition.Start_Orientation;
      Transition.Current_Projection := Transition.Start_Projection;

      Transition.Start_Time := Ada.Calendar.Clock;
      Transition.Last_Update := Transition.Start_Time;

      Transition.Completed := Transition.Transition_Time = 0.0;
   end Start;

   ------------
   -- Update --
   ------------

   procedure Update
     (Transition : in out Root_Transition_Type)
   is
      use Xi, Xi.Float_Arrays;
      use Ada.Calendar;
      Now     : constant Time := Clock;
      Elapsed : constant Duration := Now - Transition.Start_Time;
      Tick    : constant Duration := Now - Transition.Last_Update;
      Time_Progress : constant Xi_Non_Negative_Float :=
                        (if Transition.Completed
                         then 1.0
                         else Xi_Non_Negative_Float (Elapsed)
                         / Xi_Non_Negative_Float (Transition.Transition_Time));
      Distance_Progress : Xi_Unit_Float;
   begin
      if Time_Progress >= 1.0 then
         Transition.Completed := True;
         Transition.Current_Position := Transition.Target_Position;
         Transition.Current_Orientation := Transition.Target_Orientation;
         Transition.Current_Projection := Transition.Target_Projection;
         Xi.Logging.Put ("transition: complete; elapsed time ");
         Xi.Logging.Put (Xi_Float (Elapsed));
         Xi.Logging.Put_Line ("s");
      else
         if Transition.Acceleration > 0.0 then
            if Time_Progress < Transition.Coast_Start_At then
               Distance_Progress :=
                 Transition.Accel_Coefficient
                   * Time_Progress ** 2;
            elsif Time_Progress < Transition.Decel_Start_At then
               Distance_Progress :=
                 Transition.Coast_Start_Progress
                   + (Time_Progress - Transition.Coast_Start_At)
                 * Transition.Coast_Coefficient;
            else
               Distance_Progress :=
                 1.0 -
                   Transition.Accel_Coefficient
                     * (1.0 - Time_Progress) ** 2;
            end if;

            Transition.Current_Position :=
              Transition.Start_Position
                + Transition.Position_Delta * Distance_Progress;
         else
            Transition.Current_Position :=
              Transition.Start_Position
                + Transition.Position_Delta * Xi_Float (Tick);
         end if;

         Transition.Current_Orientation :=
           Transition.Start_Orientation
             + Transition.Orientation_Delta * Distance_Progress;
         Transition.Current_Projection :=
           Transition.Start_Projection
             + Transition.Projection_Delta * Distance_Progress;
      end if;

      Transition.Last_Update := Now;
      if Transition.Transit_Position then
         Transition.Scene.Active_Camera.Set_Position
           (Transition.Current_Position);
      end if;

      if Transition.Transit_Orientation then
         Transition.Scene.Active_Camera.Set_Orientation
           (Transition.Current_Orientation);
      end if;

      if Transition.Transit_Projection then
         Transition.Scene.Active_Camera.Set_Projection_Matrix
           (Transition.Current_Projection);
      end if;

   end Update;

end Concorde.Transitions;
