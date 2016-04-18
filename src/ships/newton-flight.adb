package body Newton.Flight is

   procedure Compute_Forces
     (Model    : Flight_Model'Class;
      CM_Force : out Vector_3;
      Torque   : out Vector_3);

   function Cross_Product
     (Left, Right : Vector_3)
      return Vector_3;

   function Skew_Symmetric
     (Vector : Vector_3)
      return Matrix_3;

   function Orthonormalise
     (Matrix : Matrix_3)
      return Matrix_3;

   -----------------
   -- Apply_Force --
   -----------------

   procedure Apply_Force
     (Object     : in out Mobile_Object'Class;
      Time_Delta : in Real;
      Force      : in     Vector_3)
   is
      use Ada.Numerics.Long_Real_Arrays;
   begin
      Object.Set_Velocity
        (Object.Velocity + Time_Delta / Object.Mass * Force);
   end Apply_Force;

   ------------------
   -- Apply_Torque --
   ------------------

   procedure Apply_Torque
     (Object     : in out Spinning_Object'Class;
      Time_Delta : in Real;
      Force      : in     Vector_3;
      Location   : in     Vector_3)
   is
   begin
      null;
   end Apply_Torque;

   --------------------
   -- Compute_Forces --
   --------------------

   procedure Compute_Forces
     (Model    : Flight_Model'Class;
      CM_Force : out Vector_3;
      Torque   : out Vector_3)
   is
   begin
      CM_Force := (0.0, 0.0, 0.0);
      Torque   := (0.0, 0.0, 0.0);

      for I in 1 .. Model.Engine_Count loop
         declare
            use Ada.Numerics.Long_Real_Arrays;
            Force_Magnitude : constant Real :=
                                Model.Engine (I).Power
                                * Model.Engine (I).Maximum_Thrust;
            Engine_Direction : constant Vector_3 :=
                                 Model.Engine (I).Orientation
                                 * (0.0, 0.0, 1.0);
            Force_Direction  : constant Vector_3 :=
                                 Model.Orientation
                                   * Engine_Direction;
            Force_Location   : constant Vector_3 :=
                                 Model.Orientation
                                * Model.Engine (I).Location;
         begin
            CM_Force := CM_Force + Force_Magnitude * Force_Direction;
            Torque   := Torque +
              Cross_Product (Force_Location,
                             Force_Magnitude * Force_Direction);
         end;
      end loop;
   end Compute_Forces;

   -------------------
   -- Cross_Product --
   -------------------

   function Cross_Product
     (Left, Right : Vector_3)
      return Vector_3
   is
   begin
      return (Left (2) * Right (3) - Left (3) * Right (2),
              Left (3) * Right (1) - Left (1) * Right (3),
              Left (1) * Right (2) - Left (2) * Right (1));
   end Cross_Product;

   ------------
   -- Manage --
   ------------

   procedure Manage
     (Manager : in out Flight_Manager;
      Model   : not null access Flight_Model'Class)
   is
      use Matrices;
      Engine_Torque : array (1 .. Model.Engine_Count) of Vector_3;
      Engine_Force  : array (1 .. Model.Engine_Count) of Vector_3;

      type Array_Of_Engines is array (Positive range <>) of Positive;

      procedure Try_Engines
        (Engines     : Array_Of_Engines;
         Force_Axes  : Natural;
         Torque_Axes : Natural);

      -----------------
      -- Try_Engines --
      -----------------

      procedure Try_Engines
        (Engines     : Array_Of_Engines;
         Force_Axes  : Natural;
         Torque_Axes : Natural)
      is
         pragma Assert (Torque_Axes <= 1);
         pragma Assert (Force_Axes <= 1);
         Combined_Torque : Vector_3 := (0.0, 0.0, 0.0);
         Combined_Force  : Vector_3 := (0.0, 0.0, 0.0);
         Torque_Axis_Count : Natural := 0;
         Torque_Axis       : Natural := 0;
         Force_Axis_Count  : Natural := 0;
         Force_Axis        : Natural := 0;
         pragma Unreferenced (Force_Axis);
      begin

         for E_Index of Engines loop
            Combined_Torque := Combined_Torque + Engine_Torque (E_Index);
            Combined_Force  := Combined_Force + Engine_Force (E_Index);
         end loop;

         for K in Combined_Torque'Range loop
            if abs Combined_Torque (K) > 1.0e-6 then
               Torque_Axis_Count := Torque_Axis_Count + 1;
               Torque_Axis := K;
            end if;
            if abs Combined_Force (K) > 1.0e-6 then
               Force_Axis_Count := Force_Axis_Count + 1;
               Force_Axis := K;
            end if;
         end loop;

         if Torque_Axis_Count = Torque_Axes
           and then Force_Axis_Count = Force_Axes
         then
            declare
               Negative : constant Boolean :=
                            Combined_Torque (Torque_Axis) < 0.0;
               Xform    : Transformer renames
                            Manager.Rotations (Torque_Axis, Negative);
            begin
               if Xform.Engines.Last_Index = 0 then
                  for E_Index of Engines loop
                     Xform.Engines.Append
                       ((E_Index,
                        Engine_Torque (E_Index) (Torque_Axis)
                        * Model.Engine (E_Index).Maximum_Thrust));
                  end loop;
               end if;
            end;
         end if;
      end Try_Engines;

   begin
      Manager.Model := Model;

      for I in 1 .. Model.Engine_Count loop
         declare
            Engine_Direction : constant Vector_3 :=
                                 Model.Engine (I).Orientation
                                 * (0.0, 0.0, 1.0);
            Engine_Location   : constant Vector_3 :=
                                  Model.Engine (I).Location;
            Torque : constant Vector_3 :=
                                  Cross_Product (Engine_Location,
                                                 Engine_Direction);
            Force             : constant Vector_3 :=
                                  Engine_Direction;
         begin
            Engine_Torque (I) := Torque;
            Engine_Force (I) := Force;
         end;
      end loop;

      for I in Engine_Torque'Range loop
         declare
            Current : constant Vector_3 := Engine_Torque (I);
         begin
            for J in I + 1 .. Engine_Torque'Last loop
               declare
                  Combined_Torque : constant Vector_3 :=
                                      Current + Engine_Torque (J);
                  Combined_Force  : constant Vector_3 :=
                                      Engine_Force (I) + Engine_Force (J);
                  Axis_Count      : Natural := 0;
                  Axis            : Natural := 0;
               begin

                  for K in Combined_Torque'Range loop
                     if abs Combined_Torque (K) > 1.0e-6 then
                        Axis_Count := Axis_Count + 1;
                        Axis := K;
                     end if;
                  end loop;

                  if Axis_Count = 1
                    and then abs Combined_Force < 1.0e-1
                  then
                     declare
                        Negative : constant Boolean :=
                                     Combined_Torque (Axis) < 0.0;
                        Xform    : Transformer renames
                                     Manager.Rotations (Axis, Negative);
                     begin
                        Xform.Engines.Append
                          ((I, Current (Axis)
                           * Model.Engine (I).Maximum_Thrust));
                        Xform.Engines.Append
                          ((J,
                           Engine_Torque (J) (Axis)
                           * Model.Engine (J).Maximum_Thrust));
                     end;
                  end if;
               end;
            end loop;
         end;
      end loop;

      for I in Engine_Torque'Range loop
         for J in I + 1 .. Engine_Torque'Last loop
            Try_Engines ((I, J), 1, 1);
         end loop;
      end loop;

      for I in reverse Engine_Torque'Range loop
         Try_Engines ((1 => I), 1, 1);
      end loop;

--        for I in 1 .. Model.Engine_Count loop
--           declare
--              Engine_Direction : constant Vector_3 :=
--                                   Model.Engine (I).Orientation
--                                   * (0.0, 0.0, 1.0);
--              Engine_Location   : constant Vector_3 :=
--                                    Model.Engine (I).Location;
--              Torque : constant Vector_3 :=
--                                    Cross_Product (Engine_Location,
--                                                   Engine_Direction);
--              Force  : constant Vector_3 := Engine_Direction;
--           begin
--              for Axis in Force'Range loop
--                 if Force (Axis) /= 0.0 then
--                    declare
--                       Negative : constant Boolean := Force (3) < 0.0;
--                       Xform    : Transformer renames
--                                    Manager.Accelerations (Axis, Negative);
--                    begin
--                       Xform.Engines.Append
--                         ((I, Force (3) * Model.Engine (I).Maximum_Thrust));
--                    end;
--                 end if;
--
--                 declare
--                    Non_Zero : constant Boolean := Torque (Axis) /= 0.0;
--                    Negative : constant Boolean := Torque (Axis) < 0.0;
--                    Xform    : Transformer renames
--                                 Manager.Rotations (Axis, Negative);
--                 begin
--                    if Non_Zero then
--                       Xform.Engines.Append
--                         ((I, Torque (Axis)
      --  * Model.Engine (I).Maximum_Thrust));
--                    end if;
--                 end;
--              end loop;
--           end;
--        end loop;
   end Manage;

   --------------------
   -- Orthonormalise --
   --------------------

   function Orthonormalise
     (Matrix : Matrix_3)
      return Matrix_3
   is
      use Matrices;
      X : Vector_3 := (Matrix (1, 1), Matrix (2, 1), Matrix (3, 1));
      Y : Vector_3 := (Matrix (1, 2), Matrix (2, 2), Matrix (3, 2));
      Z : Vector_3;
   begin
      X := X / abs X;
      Z := Cross_Product (X, Y);
      Z := Z / abs Z;
      Y := Cross_Product (Z, X);
      Y := Y / abs Y;

      return ((X (1), Y (1), Z (1)),
              (X (2), Y (2), Z (2)),
              (X (3), Y (3), Z (3)));
   end Orthonormalise;

   ------------
   -- Rotate --
   ------------

   procedure Rotate
     (Manager         : in out Flight_Manager;
      Target_Velocity : in Vector_3)
   is
   begin
      Manager.Target_Rotation := Target_Velocity;
   end Rotate;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
     (Object : in out Oriented_Object'Class;
      Vector : in     Vector_3;
      Angle  : in     Real)
   is
      pragma Unreferenced (Object);
      pragma Unreferenced (Vector);
      pragma Unreferenced (Angle);
   begin
      null;
   end Set_Orientation;

   ----------------
   -- Set_Target --
   ----------------

   procedure Set_Target
     (Manager     : in out Flight_Manager;
      Location    : in Vector_3;
      Orientation : in Vector_3;
      Speed       : in Real)
   is
   begin
      Manager.Target_Location    := Location;
      Manager.Target_Orientation := Orientation;
      Manager.Target_Speed       := Speed;
   end Set_Target;

   --------------------
   -- Skew_Symmetric --
   --------------------

   function Skew_Symmetric
     (Vector : Vector_3)
      return Matrix_3
   is
   begin
      return ((0.0, -Vector (3), Vector (2)),
              (Vector (3), 0.0, -Vector (1)),
              (-Vector (2), Vector (1), 0.0));
   end Skew_Symmetric;

   ------------
   -- Update --
   ------------

   procedure Update
     (Model : in out Flight_Model'Class;
      Time_Delta : Real)
   is
      use Matrices;
      CM_Force, Torque : Vector_3;
   begin

      for I in 1 .. Model.Engine_Count loop
         if Model.Engine (I).Power /= Model.Engine (I).Throttle then
            declare
               Difference : constant Real :=
                              Model.Engine (I).Throttle
                              - Model.Engine (I).Power;
               Max_Change : constant Real :=
                              Model.Engine (I).Delta_Power_Per_Second
                              * Time_Delta;
            begin
               if abs Difference < Max_Change then
                  Model.Set_Engine_Power
                    (I, Model.Engine (I).Throttle);
               else
                  Model.Set_Engine_Power
                    (I,
                     Model.Engine (I).Power +
                     (if Difference < 0.0
                        then -Max_Change
                        else Max_Change));
               end if;
            end;
         end if;
      end loop;

      Compute_Forces
        (Model, CM_Force, Torque);

      Model.Set_Centre_Of_Mass_Force (CM_Force);
      Model.Set_Torque (Torque);
      Model.Set_Velocity
        (Model.Velocity + CM_Force * Time_Delta / Model.Mass);
      Model.Set_Radial_Velocity
        (Model.Radial_Velocity + Torque * Time_Delta / Model.Mass);

      Model.Update_Location (Time_Delta);
      Model.Update_Orientation (Time_Delta);
   end Update;

   ------------
   -- Update --
   ------------

   procedure Update
     (Manager    : Flight_Manager;
      Time_Delta : Real)
   is
      pragma Unreferenced (Time_Delta);
      Model : Flight_Model'Class renames Manager.Model.all;
      Current_Rotation : constant Vector_3 := Model.Radial_Velocity;
   begin
      for I in Current_Rotation'Range loop
         if Current_Rotation (I) /= Manager.Target_Rotation (I) then
            declare
               Current : constant Real := Current_Rotation (I);
               Target : constant Real := Manager.Target_Rotation (I);
               Negative       : constant Boolean :=
                                  Current > Target;
               Required_Delta : constant Real := abs (Target - Current);
               Xform          : Transformer renames
                                  Manager.Rotations (I, Negative);
               Available      : Real := 0.0;
            begin
               for E of Xform.Engines loop
                  declare
                     use Matrices;
                     Engine : Engine_Component'Class renames
                                Model.Engine (E.Engine);
                     Orientation : constant Vector_3 :=
                                     Engine.Orientation
                                     * (0.0, 0.0, 1.0);
                     Torque      : constant Vector_3 :=
                                     Cross_Product
                                       (Engine.Location,
                                        Orientation);
                  begin
                     Available := Available
                       + Torque (I)
                       * Engine.Maximum_Thrust
                       / Model.Mass;
                  end;
               end loop;

               declare
                  Max_Delta : constant Real := abs Available;
                  Throttle  : constant Real :=
                                (if Required_Delta >= Max_Delta
                                 then 1.0
                                 else Required_Delta / Max_Delta);
               begin
                  for E of Xform.Engines loop
                     Model.Throttle_Engine (E.Engine, Throttle);
                  end loop;
               end;

               for E of
                 Manager.Rotations (I, not Negative).Engines
               loop
                  Manager.Model.Throttle_Engine (E.Engine, 0.0);
               end loop;
            end;
         end if;
      end loop;
   end Update;

   ---------------------
   -- Update_Location --
   ---------------------

   procedure Update_Location
     (Object : in out Mobile_Object'Class;
      Time_Delta : in Real)
   is
      use Ada.Numerics.Long_Real_Arrays;
   begin
      Object.Set_Location (Object.Location + Time_Delta * Object.Velocity);
   end Update_Location;

   ------------------------
   -- Update_Orientation --
   ------------------------

   procedure Update_Orientation
     (Object : in out Spinning_Object'Class;
      Time_Delta : in Real)
   is
      use Matrices;
      Skew_Matrix : constant Matrix_3 :=
                      Skew_Symmetric (Object.Radial_Velocity);
      Delta_Orientation : constant Matrix_3 :=
                            Time_Delta * Skew_Matrix * Object.Orientation;
      New_Orientation   : constant Matrix_3 :=
                            Object.Orientation + Delta_Orientation;
      Norm_Orientation  : constant Matrix_3 :=
                            Orthonormalise (New_Orientation);
   begin
      Object.Set_Orientation (Norm_Orientation);
   end Update_Orientation;

end Newton.Flight;
