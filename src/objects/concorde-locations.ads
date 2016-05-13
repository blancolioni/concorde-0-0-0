with Newton;

with Concorde.Geometry;

with Concorde.Objects;
limited with Concorde.Systems;

package Concorde.Locations is

   type Location_Type is
     (Nowhere,
      Interstellar, Orbit,
      World_Surface, On_Ship, In_Unit);

   type Object_Location (Loc_Type : Location_Type := Nowhere) is private;

   function Interstellar_Location
     (System_1, System_2 : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Progress           : Unit_Real)
      return Object_Location;

   function Orbit
     (Primary        : not null access constant
        Concorde.Objects.Root_Object_Type'Class;
      Position       : Newton.Vector_3;
      Velocity       : Newton.Vector_3)
      return Object_Location;

   type Located_Interface is interface;

   function Current_Location
     (Located : Located_Interface)
      return Object_Location
      is abstract;

   procedure Set_Location
     (Located : in out Located_Interface;
      Location : Object_Location)
   is abstract;

private

   type Object_Location (Loc_Type : Location_Type := Nowhere) is
      record
         Reference : Concorde.Objects.Object_Type;
         case Loc_Type is
            when Nowhere =>
               null;
            when Interstellar =>
               Destination_System : Concorde.Objects.Object_Type;
               Progress           : Unit_Real;
            when Orbit =>
               Angle              : Concorde.Geometry.Radians;
               Apoapsis           : Newton.Vector_3;
               Periapsis          : Newton.Vector_3;
               Offset             : Unit_Real;
               Clockwise          : Boolean;
            when World_Surface =>
               Sector             : Positive;
            when On_Ship =>
               Module             : Positive;
            when In_Unit =>
               null;
         end case;
      end record;

end Concorde.Locations;
