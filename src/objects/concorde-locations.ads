with Newton;

with Concorde.Dates;
with Concorde.Geometry;

with Concorde.Objects;
limited with Concorde.Systems;

package Concorde.Locations is

   type Location_Type is
     (Nowhere,
      Interstellar, System_Point, Orbit,
      World_Surface, On_Ship, At_Installation, In_Unit);

   type Object_Location (Loc_Type : Location_Type := Nowhere) is private;

   function Short_Name (Location : Object_Location) return String;
   function Long_Name (Location : Object_Location) return String;

   function Primary
     (Location : Object_Location)
      return Concorde.Objects.Object_Type;

   function Primary_Relative_Position
     (Location : Object_Location)
      return Newton.Vector_3;

   function Current_System
     (Location : Object_Location)
      return access constant Concorde.Systems.Root_Star_System_Type'Class;

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
     (System : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Object_Location;

   function Geosynchronous_Orbit
     (Primary        : not null access constant
        Concorde.Objects.Massive_Object_Interface'Class)
      return Object_Location;

   function Orbit
     (Primary        : not null access constant
        Concorde.Objects.Massive_Object_Interface'Class;
      Altitude       : Real)
      return Object_Location;

   function System_Point
     (Primary           : not null access constant
        Concorde.Systems.Star_System_Object_Interface'Class;
      Relative_Position : Newton.Vector_3;
      Relative_Velocity : Newton.Vector_3)
      return Object_Location;

   function World_Surface
     (World  : not null access constant
        Concorde.Objects.Root_Object_Type'Class;
      Sector : Positive)
      return Object_Location;

   function World_Sector
     (Location : Object_Location)
      return Positive;

   function At_Installation
     (Installation : not null access constant
        Concorde.Objects.Root_Object_Type'Class)
      return Object_Location;

   type Located_Interface is limited interface;

   function Current_Location
     (Located : Located_Interface)
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

   function Primary_Relative_Position
     (Located : Located_Interface'Class)
      return Newton.Vector_3
   is (Primary_Relative_Position (Located.Current_Location));

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
               Start_Time         : Concorde.Dates.Date_Type;
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
