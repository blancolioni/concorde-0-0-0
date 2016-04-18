private with Ada.Containers.Vectors;
with Ada.Numerics.Long_Real_Arrays;

package Newton.Flight is

   package Matrices renames Ada.Numerics.Long_Real_Arrays;

   subtype Vector_3 is Matrices.Real_Vector (1 .. 3);

   subtype Matrix_3 is Matrices.Real_Matrix (1 .. 3, 1 .. 3);

   subtype Vector_4 is Matrices.Real_Vector (1 .. 4);

   subtype Matrix_4 is Matrices.Real_Matrix (1 .. 4, 1 .. 4);

   type Massive_Object is interface;

   function Mass (Object : Massive_Object) return Real is abstract;

   type Oriented_Object is interface;

   function Orientation (Object : Oriented_Object) return Matrix_3
                         is abstract;

   procedure Set_Orientation
     (Object : in out Oriented_Object;
      Orientation : Matrix_3)
   is abstract;

   procedure Set_Orientation
     (Object : in out Oriented_Object'Class;
      Vector : in     Vector_3;
      Angle  : in     Real);

   type Located_Object is interface;

   function Location (Object : Located_Object) return Vector_3
                      is abstract;

   procedure Set_Location
     (Object : in out Located_Object;
      Location : Vector_3)
   is abstract;

   type Mobile_Object is interface
     and Massive_Object and Located_Object;

   function Velocity (Object : Mobile_Object) return Vector_3
                      is abstract;

   procedure Set_Velocity
     (Object : in out Mobile_Object;
      Velocity : Vector_3)
   is abstract;

   function Centre_Of_Mass_Force
     (Object : Mobile_Object)
      return Vector_3
      is abstract;

   procedure Set_Centre_Of_Mass_Force
     (Object : in out Mobile_Object;
      Force : Vector_3)
   is abstract;

   procedure Update_Location
     (Object : in out Mobile_Object'Class;
      Time_Delta : in Real);

   procedure Apply_Force (Object     : in out Mobile_Object'Class;
                          Time_Delta : in Real;
                          Force      : in     Vector_3);

   type Spinning_Object is interface
     and Massive_Object and Oriented_Object;

   function Radial_Velocity
     (Object : Spinning_Object) return Vector_3
      is abstract;

   procedure Set_Radial_Velocity
     (Object : in out Spinning_Object;
      Velocity : Vector_3)
   is abstract;

   function Torque
     (Object : Spinning_Object) return Vector_3
      is abstract;

   procedure Set_Torque
     (Object : in out Spinning_Object;
      Torque : Vector_3)
   is abstract;

   procedure Update_Orientation
     (Object     : in out Spinning_Object'Class;
      Time_Delta : in Real);

   procedure Apply_Torque
     (Object     : in out Spinning_Object'Class;
      Time_Delta : in Real;
      Force      : in     Vector_3;
      Location   : in     Vector_3);

   type Component_Object is interface and Located_Object;

   type Oriented_Component_Object is interface
     and Oriented_Object and Component_Object;

   type Throttled_Component_Object is interface
     and Component_Object;

   procedure Set_Throttle (Object  : in out Throttled_Component_Object;
                           Setting : in     Unit_Real)
   is abstract;

   procedure Set_Power (Object  : in out Throttled_Component_Object;
                        Setting : in     Unit_Real)
   is abstract;

   function Throttle (Object : Throttled_Component_Object)
                      return Unit_Real
                      is abstract;

   function Power (Object : Throttled_Component_Object)
                   return Unit_Real
                   is abstract;

   function Delta_Power_Per_Second
     (Object : Throttled_Component_Object)
      return Unit_Real
      is abstract;

   type Engine_Component is interface
     and Throttled_Component_Object
     and Oriented_Component_Object;

   function Maximum_Thrust
     (Engine : Engine_Component)
      return Real
      is abstract;

   type Flight_Model is interface
     and Massive_Object
     and Oriented_Object
     and Located_Object
     and Mobile_Object
     and Spinning_Object;

   function Engine_Count (Model : Flight_Model) return Natural
                          is abstract;

   function Engine (Model : Flight_Model;
                    Index : Positive)
                    return Engine_Component'Class
                    is abstract;

   procedure Set_Engine_Power
     (Model   : in out Flight_Model;
      Index   : Positive;
      Setting : Unit_Real)
   is abstract;

   procedure Throttle_Engine
     (Model   : in out Flight_Model;
      Index   : Positive;
      Setting : Unit_Real)
   is abstract;

   procedure Update
     (Model      : in out Flight_Model'Class;
      Time_Delta : Real);

   type Flight_Manager is tagged private;

   procedure Manage (Manager : in out Flight_Manager;
                     Model   : not null access Flight_Model'Class);

   procedure Set_Target
     (Manager     : in out Flight_Manager;
      Location    : in Vector_3;
      Orientation : in Vector_3;
      Speed       : in Real);

   procedure Update (Manager    : Flight_Manager;
                     Time_Delta : Real);

   procedure Rotate
     (Manager         : in out Flight_Manager;
      Target_Velocity : in Vector_3);

private

   type Managed_Engine_Record is
      record
         Engine : Positive;
         Relative_Throttle : Real;
      end record;

   package Managed_Engine_Vectors is
     new Ada.Containers.Vectors (Positive, Managed_Engine_Record);

   type Transformer is
      record
         Engines : Managed_Engine_Vectors.Vector;
      end record;

   subtype Axis_Index is Positive range 1 .. 3;

   type Delta_Rotation_Array is
     array (Axis_Index, Boolean) of Transformer;

   type Delta_Velocity_Array is
     array (Axis_Index, Boolean) of Transformer;

   type Flight_Manager is tagged
      record
         Model              : access Flight_Model'Class;
         Rotations          : Delta_Rotation_Array;
         Accelerations      : Delta_Velocity_Array;
         Target_Location    : Vector_3 := (0.0, 0.0, 0.0);
         Target_Orientation : Vector_3 := (0.0, 0.0, 0.0);
         Target_Speed       : Real     := 0.0;
         Target_Rotation    : Vector_3 := (0.0, 0.0, 0.0);
      end record;

end Newton.Flight;
