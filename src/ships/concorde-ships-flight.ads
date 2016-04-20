private with Ada.Containers.Vectors;

with Newton.Flight;

package Concorde.Ships.Flight is

   type Root_Newtonian_Ship is
     new Newton.Flight.Flight_Model with private;

   overriding function Location
     (Ship : Root_Newtonian_Ship)
      return Newton.Flight.Vector_3;

   overriding function Orientation
     (Ship : Root_Newtonian_Ship)
      return Newton.Flight.Matrix_3;

   function Ship
     (Newton_Ship : Root_Newtonian_Ship'Class)
      return Ship_Type;

   function Engine_Mount
     (Ship         : Root_Newtonian_Ship'Class;
      Engine_Index : Positive)
      return Mounted_Module;

   function Selected_Component_Index
     (Ship : Root_Newtonian_Ship'Class)
      return Natural;

   procedure Select_Component
     (Ship   : in out Root_Newtonian_Ship'Class;
      Index  : Natural;
      Toggle : Boolean := False);

   type Newtonian_Ship is access all Root_Newtonian_Ship'Class;

   function Create_Newtonian_Ship
     (From_Ship   : Concorde.Ships.Ship_Type;
      Location    : Newton.Flight.Vector_3;
      Velocity    : Newton.Flight.Vector_3;
      Orientation : Newton.Flight.Matrix_3)
      return Newtonian_Ship;

   procedure Add_Engine_Mount
     (Ship         : in out Root_Newtonian_Ship'Class;
      Mount        : Mounted_Module;
      Location     : Newton.Flight.Vector_3;
      Orientation  : Newton.Flight.Matrix_3;
      Max_Thrust   : Non_Negative_Real;
      Delta_Thrust : Unit_Real);

private

   type Ship_Engine is new Newton.Flight.Engine_Component with
      record
         Mount        : Mounted_Module;
         Location     : Newton.Flight.Vector_3;
         Orientation  : Newton.Flight.Matrix_3;
         Max_Thrust   : Real;
         Throttle     : Unit_Real := 0.0;
         Power        : Unit_Real := 0.0;
         Delta_Thrust : Real;
      end record;

   overriding function Location
     (Engine : Ship_Engine)
      return Newton.Flight.Vector_3
   is (Engine.Location);

   overriding function Orientation
     (Engine : Ship_Engine)
      return Newton.Flight.Matrix_3
   is (Engine.Orientation);

   overriding procedure Set_Location
     (Engine   : in out Ship_Engine;
      Location : Newton.Flight.Vector_3);

   overriding procedure Set_Orientation
     (Engine      : in out Ship_Engine;
      Orientation : Newton.Flight.Matrix_3);

   overriding function Throttle
     (Engine : Ship_Engine)
      return Unit_Real
   is (Engine.Throttle);

   overriding function Power
     (Engine : Ship_Engine)
      return Unit_Real
   is (Engine.Power);

   overriding function Delta_Power_Per_Second
     (Engine : Ship_Engine)
      return Unit_Real
   is (Engine.Delta_Thrust);

   overriding procedure Set_Throttle
     (Engine   : in out Ship_Engine;
      Throttle : Unit_Real);

   overriding procedure Set_Power
     (Engine : in out Ship_Engine;
      Power  : Unit_Real);

   overriding function Maximum_Thrust
     (Engine : Ship_Engine)
      return Real
   is (Engine.Max_Thrust);

   package Engine_Vectors is
     new Ada.Containers.Vectors (Positive, Ship_Engine);

   type Root_Newtonian_Ship is
     new Newton.Flight.Flight_Model with
      record
         Ship            : Concorde.Ships.Ship_Type;
         Selected_Index  : Natural := 0;
         Mass            : Non_Negative_Real;
         Engines         : Engine_Vectors.Vector;
         Location        : Newton.Flight.Vector_3 := (0.0, 0.0, 0.0);
         Orientation     : Newton.Flight.Matrix_3 :=
                             Newton.Flight.Matrices.Unit_Matrix (3);
         Velocity        : Newton.Flight.Vector_3 := (0.0, 0.0, 0.0);
         CM_Force        : Newton.Flight.Vector_3 := (0.0, 0.0, 0.0);
         Radial_Velocity : Newton.Flight.Vector_3 := (0.0, 0.0, 0.0);
         Torque          : Newton.Flight.Vector_3 := (0.0, 0.0, 0.0);
      end record;

   overriding function Mass
     (Ship : Root_Newtonian_Ship)
      return Real
   is (Ship.Mass);

   overriding function Velocity
     (Ship : Root_Newtonian_Ship)
      return Newton.Flight.Vector_3
   is (Ship.Velocity);

   overriding function Centre_Of_Mass_Force
     (Ship : Root_Newtonian_Ship)
      return Newton.Flight.Vector_3
   is (Ship.CM_Force);

   overriding function Radial_Velocity
     (Ship : Root_Newtonian_Ship)
      return Newton.Flight.Vector_3
   is (Ship.Radial_Velocity);

   overriding function Torque
     (Ship : Root_Newtonian_Ship)
      return Newton.Flight.Vector_3
   is (Ship.Torque);

   overriding procedure Set_Orientation
     (Ship        : in out Root_Newtonian_Ship;
      Orientation : Newton.Flight.Matrix_3);

   overriding procedure Set_Location
     (Ship     : in out Root_Newtonian_Ship;
      Location : Newton.Flight.Vector_3);

   overriding procedure Set_Velocity
     (Ship     : in out Root_Newtonian_Ship;
      Velocity : Newton.Flight.Vector_3);

   overriding procedure Set_Centre_Of_Mass_Force
     (Ship  : in out Root_Newtonian_Ship;
      Force : Newton.Flight.Vector_3);

   overriding procedure Set_Radial_Velocity
     (Ship     : in out Root_Newtonian_Ship;
      Velocity : Newton.Flight.Vector_3);

   overriding procedure Set_Torque
     (Ship   : in out Root_Newtonian_Ship;
      Torque : Newton.Flight.Vector_3);

   overriding function Engine_Count
     (Ship : Root_Newtonian_Ship)
      return Natural;

   overriding function Engine
     (Ship  : Root_Newtonian_Ship;
      Index : Positive)
      return Newton.Flight.Engine_Component'Class;

   overriding procedure Throttle_Engine
     (Ship    : in out Root_Newtonian_Ship;
      Index   : Positive;
      Setting : Unit_Real);

   overriding procedure Set_Engine_Power
     (Ship    : in out Root_Newtonian_Ship;
      Index   : Positive;
      Setting : Unit_Real);

end Concorde.Ships.Flight;
