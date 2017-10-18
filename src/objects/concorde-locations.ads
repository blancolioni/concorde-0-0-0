with Newton;

with Concorde.Calendar;
with Concorde.Geometry;

with Concorde.Objects;
limited with Concorde.Systems;
limited with Concorde.Worlds;

package Concorde.Locations is

   type Location_Type is
     (Nowhere,
      Interstellar, System_Point, Orbit,
      World_Surface, On_Ship, At_Installation, In_Unit);

   type Object_Location (Loc_Type : Location_Type := Nowhere) is private;

   function Short_Name (Location : Object_Location) return String;
   function Long_Name (Location : Object_Location) return String;

   function Location_At
     (Location : Object_Location;
      Time     : Concorde.Calendar.Time)
      return Object_Location;

   function Primary
     (Location : Object_Location)
      return Concorde.Objects.Object_Type;

   function Primary_Relative_Position
     (Location : Object_Location;
      Time     : Concorde.Calendar.Time)
      return Newton.Vector_3;

   function System_Relative_Position
     (Location : Object_Location;
      Time     : Concorde.Calendar.Time)
      return Newton.Vector_3;

   function Current_System
     (Location : Object_Location)
      return access constant Concorde.Systems.Root_Star_System_Type'Class;

   function Is_World_Location
     (Location : Object_Location)
      return Boolean;

   function Current_World
     (Location : Object_Location)
      return access constant Concorde.Worlds.Root_World_Type'Class
     with Pre => Is_World_Location (Location);

   subtype Orbital_Location is Object_Location (Orbit);
   subtype System_Point_Location is Object_Location (System_Point);

   function Get_Orbit_Location
     (Orbit_Loc : Orbital_Location;
      Time      : Concorde.Calendar.Time)
      return System_Point_Location;

   function To_System_Point
     (Loc  : Object_Location;
      Time : Concorde.Calendar.Time)
      return System_Point_Location;

   function Nowhere return Object_Location;

   function Interstellar_Location
     (System_1, System_2 : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Progress           : Unit_Real)
      return Object_Location;

   function Orbit
     (Primary        : not null access constant
        Concorde.Objects.Massive_Object_Interface'Class;
      Position       : Newton.Vector_3;
      Velocity       : Newton.Vector_3)
      return Object_Location;

   function System_Transfer_Orbit
     (From_System, To_System : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Object_Location;

   function Geosynchronous_Orbit
     (Primary        : not null access constant
        Concorde.Objects.Massive_Object_Interface'Class)
      return Object_Location;

   function Altitude_Orbit
     (Primary        : not null access constant
        Concorde.Systems.Star_System_Object_Interface'Class;
      Altitude       : Non_Negative_Real)
      return Object_Location;

   function Circular_Orbit
     (Primary        : not null access constant
        Concorde.Objects.Massive_Object_Interface'Class;
      Radius         : Non_Negative_Real)
      return Object_Location;

   function System_Point
     (System            : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Relative_Position : Newton.Vector_3;
      Relative_Velocity : Newton.Vector_3)
      return Object_Location;

   function World_Surface
     (World  : not null access constant
        Concorde.Objects.Root_Object_Type'Class;
      Sector : Positive)
      return Object_Location;

   function Intermediate_Location
     (Start, Finish : Object_Location;
      Progress      : Unit_Real)
      return Object_Location;

   function World_Sector
     (Location : Object_Location)
      return Positive;

   function At_Installation
     (Installation : not null access constant
        Concorde.Objects.Root_Object_Type'Class)
      return Object_Location;

   function System_Distance
     (From, To : Object_Location)
      return Non_Negative_Real;

   type Located_Interface is limited interface;

   function Current_Location
     (Located : Located_Interface)
      return Object_Location
      is abstract;

   function Location_At
     (Located : Located_Interface;
      Time    : Concorde.Calendar.Time)
      return Object_Location
      is abstract;

   procedure Set_Location
     (Located : in out Located_Interface;
      Location : Object_Location)
   is abstract;

   function Orbiting
     (Located : Located_Interface'Class;
      Primary : not null access constant
        Concorde.Objects.Root_Object_Type'Class)
      return Boolean;

   function Orbiting
     (Located : Located_Interface'Class)
      return access constant Concorde.Objects.Root_Object_Type'Class;

   function Current_System
     (Located : Located_Interface'Class)
      return access constant Concorde.Systems.Root_Star_System_Type'Class
   is (Current_System (Located.Current_Location));

   function Orbiting_World
     (Located : Located_Interface'Class)
      return Boolean;

   function Is_World_Location
     (Located : Located_Interface'Class)
      return Boolean
   is (Is_World_Location (Located.Current_Location));

   function Current_World
     (Located : Located_Interface'Class)
      return access constant Concorde.Worlds.Root_World_Type'Class
   is (Current_World (Located.Current_Location))
     with Pre => Located.Is_World_Location;

   function System_Relative_Position
     (Located  : Located_Interface'Class;
      Time     : Concorde.Calendar.Time)
      return Newton.Vector_3;

   function Primary_Relative_Position
     (Located  : Located_Interface'Class;
      Time     : Concorde.Calendar.Time)
      return Newton.Vector_3
   is (Primary_Relative_Position (Located.Current_Location, Time));

private

   type Object_Location (Loc_Type : Location_Type := Nowhere) is
      record
         Reference : access constant Concorde.Objects.Root_Object_Type'Class;
         case Loc_Type is
            when Nowhere =>
               null;
            when Interstellar =>
               Destination_System : access constant
                 Concorde.Objects.Root_Object_Type'Class;
               Progress           : Unit_Real;
            when System_Point =>
               Relative_Position  : Newton.Vector_3;
               Relative_Velocity  : Newton.Vector_3;
            when Orbit =>
               Angle              : Concorde.Geometry.Radians;
               Apoapsis           : Newton.Vector_3;
               Periapsis          : Newton.Vector_3;
               Start_Time         : Concorde.Calendar.Time;
               Start_Offset       : Unit_Real;
               Period             : Duration;
               Clockwise          : Boolean;
            when World_Surface =>
               Sector             : Positive;
            when At_Installation =>
               null;
            when On_Ship =>
               Module             : Positive;
            when In_Unit =>
               null;
         end case;
      end record;

end Concorde.Locations;
