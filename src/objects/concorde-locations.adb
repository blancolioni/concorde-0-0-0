with Ada.Exceptions;
with Ada.Numerics;
with Ada.Text_IO;

with Concorde.Constants;
with Concorde.Solar_System;
with Concorde.Elementary_Functions;
with Concorde.Random;
with Concorde.Real_Images;

with Concorde.Installations;
with Concorde.People.Communities;
with Concorde.Ships;
with Concorde.Stars;
with Concorde.Systems;
with Concorde.Worlds;

package body Concorde.Locations is

   --------------------
   -- Altitude_Orbit --
   --------------------

   function Altitude_Orbit
     (Primary        : not null access constant
        Concorde.Systems.Star_System_Object_Interface'Class;
      Altitude       : Non_Negative_Real)
      return Object_Location
   is
      Primary_Radius : constant Non_Negative_Real := Primary.Radius;
      Radius         : constant Non_Negative_Real :=
                         Primary_Radius + Altitude;
      Result         : constant Object_Location :=
                         Circular_Orbit (Primary, Radius);
   begin
      return Result;
   end Altitude_Orbit;

   ---------------------
   -- At_Installation --
   ---------------------

   function At_Installation
     (Installation : not null access constant
        Concorde.Objects.Root_Object_Type'Class)
      return Object_Location
   is
   begin
      return (At_Installation, Installation);
   end At_Installation;

   --------------------
   -- Circular_Orbit --
   --------------------

   function Circular_Orbit
     (Primary        : not null access constant
        Concorde.Objects.Massive_Object_Interface'Class;
      Radius         : Non_Negative_Real)
      return Object_Location
   is
      use Concorde.Elementary_Functions;
      V       : constant Non_Negative_Real :=
                  Sqrt (Concorde.Constants.Gravitational_Constant
                        * Primary.Mass / Radius);
   begin
      return Orbit (Primary,
                    Position => (Radius, 0.0, 0.0),
                    Velocity => (0.0, V, 0.0));
   end Circular_Orbit;

   --------------------
   -- Current_System --
   --------------------

   function Current_System
     (Location : Object_Location)
      return access constant Concorde.Systems.Root_Star_System_Type'Class
   is
   begin
      case Location.Loc_Type is
         when Nowhere =>
            return null;
         when Interstellar =>
            return null;
         when System_Point =>
            return Concorde.Systems.Star_System_Type (Location.Reference);
         when Orbit | World_Surface | In_Community =>
            return Concorde.Systems.Star_System_Object_Interface'Class
              (Location.Reference.all).System;
         when On_Ship =>
            return Current_System
              (Concorde.Ships.Ship_Type (Location.Reference).Current_Location);
         when At_Installation =>
            return Current_System
              (Concorde.Installations.Installation_Type
                 (Location.Reference).Current_Location);
         when In_Unit =>
            return null;
      end case;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "error converting " & Long_Name (Location)
            & " to a system point: "
            & Ada.Exceptions.Exception_Message (E));
         raise;
   end Current_System;

   -----------------------
   -- Current_Community --
   -----------------------

   function Current_Community
     (Location : Object_Location)
      return access constant
     Concorde.People.Communities.Root_Community_Type'Class
   is (if Location.Reference.all in
         Concorde.People.Communities.Root_Community_Type'Class
       then Concorde.People.Communities.Root_Community_Type
         (Location.Reference.all)'Access
         else Located_Interface'Class (Location.Reference.all)
       .Current_Community);

   -------------------
   -- Current_World --
   -------------------

   function Current_World
     (Location : Object_Location)
      return access constant Concorde.Worlds.Root_World_Type'Class
   is (if Location.Reference.all in Concorde.Worlds.Root_World_Type'Class
       then Concorde.Worlds.Root_World_Type'Class
         (Location.Reference.all)'Access
         else Located_Interface'Class (Location.Reference.all).Current_World);

   --------------------------
   -- Geosynchronous_Orbit --
   --------------------------

   function Geosynchronous_Orbit
     (Primary        : not null access constant
        Concorde.Objects.Massive_Object_Interface'Class)
      return Object_Location
   is
      use Concorde.Elementary_Functions;
      World   : constant Concorde.Worlds.World_Type :=
                  Concorde.Worlds.World_Type (Primary);
      R_Cubed : constant Non_Negative_Real :=
                  Concorde.Constants.Gravitational_Constant
                    * World.Mass * World.Day_Length ** 2
                  / (4.0 * Ada.Numerics.Pi);
      R       : constant Non_Negative_Real :=
                  Non_Negative_Real'Max
                    (Exp (Log (R_Cubed) / 3.0),
                     World.Radius + 100.0);
      V       : constant Non_Negative_Real :=
                  2.0 * Ada.Numerics.Pi * R
                    / World.Day_Length;
   begin
      return Orbit (Primary,
                    Position => (R, 0.0, 0.0),
                    Velocity => (0.0, V, 0.0));
   end Geosynchronous_Orbit;

   ------------------------
   -- Get_Orbit_Location --
   ------------------------

   function Get_Orbit_Location
     (Orbit_Loc : Orbital_Location;
      Time      : Concorde.Calendar.Time)
      return System_Point_Location
   is
      use Concorde.Vectors;
      Primary_Position : constant Object_Location :=
                           To_System_Point
                             (Located_Interface'Class
                                (Orbit_Loc.Reference.all).Location_At (Time),
                              Time);
      Orbit_Position   : constant Concorde.Vectors.Vector_3 :=
                           Primary_Relative_Position
                             (Orbit_Loc, Time);
   begin
      return (System_Point,
              Located_Interface'Class
                (Orbit_Loc.Reference.all).Current_System,
              Relative_Position =>
                Primary_Position.Relative_Position
              + Orbit_Position,
              Relative_Velocity =>
                Primary_Position.Relative_Velocity);
   end Get_Orbit_Location;

   ------------------
   -- In_Community --
   ------------------

   function In_Community
     (Community : not null access constant
        Concorde.Objects.Root_Object_Type'Class)
      return Object_Location
   is
   begin
      return (In_Community, Community);
   end In_Community;

   ---------------------------
   -- Intermediate_Location --
   ---------------------------

   function Intermediate_Location
     (Start, Finish : Object_Location;
      Progress      : Unit_Real)
      return Object_Location
   is
   begin
      if Start.Loc_Type = Interstellar then
         pragma Assert (Finish.Loc_Type = Interstellar);
         return (Start with delta Progress => Progress);
      end if;

      declare
         use Concorde.Systems;
         Start_System  : constant Star_System_Type :=
                           Current_System (Start);
         Finish_System : constant Star_System_Type :=
                           Current_System (Finish);
      begin
         if Start_System /= Finish_System then
            return Interstellar_Location
              (Start_System, Finish_System, Progress);
         end if;

         declare
            use Concorde.Vectors;
            Start_Loc  : constant Object_Location :=
                           To_System_Point (Start, Concorde.Calendar.Clock);
            Finish_Loc : constant Object_Location :=
                           To_System_Point (Finish, Concorde.Calendar.Clock);
            Position   : constant Concorde.Vectors.Vector_3 :=
                           Start_Loc.Relative_Position
                             + Progress * (Finish_Loc.Relative_Position
                                           - Start_Loc.Relative_Position);
         begin
            return Object_Location'
              (Loc_Type           => System_Point,
               Reference          => Start_System,
               Relative_Position  => Position,
               Relative_Velocity  => Finish_Loc.Relative_Velocity);
         end;
      end;
   end Intermediate_Location;

   ---------------------------
   -- Interstellar_Location --
   ---------------------------

   function Interstellar_Location
     (System_1, System_2 : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Progress           : Unit_Real)
      return Object_Location
   is
   begin
      return (Interstellar,
              Concorde.Objects.Object_Type (System_1),
              Concorde.Objects.Object_Type (System_2),
              Progress);
   end Interstellar_Location;

   ---------------------------
   -- Is_Community_Location --
   ---------------------------

   function Is_Community_Location
     (Location : Object_Location)
      return Boolean
   is
   begin
      return Location.Loc_Type /= Nowhere
        and then (Location.Reference.all in
                    Concorde.People.Communities.Root_Community_Type'Class
                  or else (Location.Reference.all in
                               Located_Interface'Class
                           and then Located_Interface'Class
                             (Location.Reference.all).Is_Community_Location));
   end Is_Community_Location;

   -----------------------
   -- Is_World_Location --
   -----------------------

   function Is_World_Location
     (Location : Object_Location)
      return Boolean
   is
   begin
      return Location.Loc_Type /= Nowhere
        and then (Location.Reference.all in
                    Concorde.Worlds.Root_World_Type'Class
                  or else (Location.Reference.all in
                               Located_Interface'Class
                           and then Located_Interface'Class
                             (Location.Reference.all).Is_World_Location));
   end Is_World_Location;

   -----------------
   -- Location_At --
   -----------------

   function Location_At
     (Location : Object_Location;
      Time     : Concorde.Calendar.Time)
      return Object_Location
   is
   begin
      case Location.Loc_Type is
         when Nowhere =>
            return Location;
         when Interstellar =>
            return Location;
         when System_Point =>
            return Location;
         when Orbit =>
            return Get_Orbit_Location (Location, Time);
         when World_Surface =>
            return Location;
         when In_Community =>
            return Location;
         when On_Ship =>
            return Location;
         when At_Installation =>
            return Location;
         when In_Unit =>
            return Location;
      end case;
   end Location_At;

   ---------------
   -- Long_Name --
   ---------------

   function Long_Name (Location : Object_Location) return String is
   begin
      case Location.Loc_Type is
         when Nowhere =>
            return "nowhere";
         when Interstellar =>
            return Concorde.Systems.Star_System_Type (Location.Reference).Name
              & " -> "
              & Concorde.Systems.Star_System_Type
              (Location.Destination_System).Name;
         when System_Point =>
            declare
               use Concorde.Vectors;
            begin
               return Concorde.Real_Images.Approximate_Image
                 (abs (Location.Relative_Position
                  / Concorde.Solar_System.Earth_Orbit))
                 & " AU from "
                 & Location.Reference.Identifier;
            end;
         when Orbit =>
            return "orbiting "
              & Concorde.Systems.Star_System_Object_Interface'Class
              (Location.Reference.all).Name;
         when World_Surface =>
            return "on "
              & Concorde.Worlds.World_Type (Location.Reference).Name;
         when In_Community =>
            return "in community on "
              & Concorde.People.Communities.Community_Type (Location.Reference)
              .World.Name;
         when On_Ship =>
            return "on "
              & Concorde.Ships.Ship_Type (Location.Reference).Name;
         when At_Installation =>
            return "at "
              & Concorde.Installations.Installation_Type
              (Location.Reference).Facility.Name
              & " on "
              & Current_World (Location).Name;
         when In_Unit =>
            return "in unit";
      end case;
   end Long_Name;

   -------------
   -- Nowhere --
   -------------

   function Nowhere return Object_Location is
   begin
      return (Loc_Type => Nowhere, Reference => null);
   end Nowhere;

   -----------
   -- Orbit --
   -----------

   function Orbit
     (Primary        : not null access constant
        Concorde.Objects.Massive_Object_Interface'Class;
      Position       : Concorde.Vectors.Vector_3;
      Velocity       : Concorde.Vectors.Vector_3)
      return Object_Location
   is
      pragma Unreferenced (Velocity);
      use Concorde.Vectors;
      use Concorde.Elementary_Functions;
      Pi : constant := Ada.Numerics.Pi;
      Radius : constant Non_Negative_Real := abs Position;
      Period : constant Non_Negative_Real :=
                 Sqrt (4.0 * Pi * Pi * Radius ** 3
                       / Concorde.Constants.Gravitational_Constant
                       / Primary.Mass);
   begin
      return (Orbit,
              Concorde.Objects.Root_Object_Type'Class (Primary.all)'Access,
              Geometry.Degrees_To_Radians (0.0),
              Position, -Position,
              Concorde.Calendar.Clock,
              Concorde.Random.Unit_Random, Duration (Period), True);
   end Orbit;

   --------------
   -- Orbiting --
   --------------

   function Orbiting
     (Located : Located_Interface'Class;
      Primary : not null access constant
        Concorde.Objects.Root_Object_Type'Class)
      return Boolean
   is
      Loc : Object_Location renames Located.Current_Location;
   begin
      return Loc.Loc_Type = Orbit and then Loc.Reference = Primary;
   end Orbiting;

   --------------
   -- Orbiting --
   --------------

   function Orbiting
     (Located : Located_Interface'Class)
      return access constant Concorde.Objects.Root_Object_Type'Class
   is
   begin
      return Located.Current_Location.Reference;
   end Orbiting;

   --------------------
   -- Orbiting_World --
   --------------------

   function Orbiting_World
     (Located : Located_Interface'Class)
      return Boolean
   is (Located.Current_Location.Loc_Type = Orbit
       and then Located.Current_Location.Reference.all
       in Concorde.Worlds.Root_World_Type'Class);

   -------------
   -- Primary --
   -------------

   function Primary
     (Location : Object_Location)
      return Concorde.Objects.Object_Type
   is
   begin
      return Location.Reference;
   end Primary;

   -------------------------------
   -- Primary_Relative_Position --
   -------------------------------

   function Primary_Relative_Position
     (Location : Object_Location;
      Time     : Concorde.Calendar.Time)
      return Concorde.Vectors.Vector_3
   is
   begin
      if Location.Loc_Type = System_Point then
         return Location.Relative_Position;
      else
         declare
            use Concorde.Vectors;
            use type Concorde.Calendar.Time;
            use Concorde.Geometry;
            R            : constant Non_Negative_Real :=
                             abs (Location.Apoapsis);
            Start        : constant Non_Negative_Real :=
                             Location.Start_Offset;
            Elapsed      : constant Duration :=
                             Time - Location.Start_Time;
            Total_Orbits : constant Real :=
                             Start + Real (Elapsed) / Real (Location.Period);
            Theta        : constant Radians :=
                             Degrees_To_Radians (Total_Orbits * 360.0);
         begin
            return (R * Cos (Theta), 0.0, R * Sin (Theta));
         end;
      end if;
   end Primary_Relative_Position;

   ----------------
   -- Short_Name --
   ----------------

   function Short_Name (Location : Object_Location) return String is
   begin
      case Location.Loc_Type is
         when Nowhere =>
            return "nowhere";
         when Interstellar =>
            return Concorde.Systems.Star_System_Type (Location.Reference).Name
              & " -> "
              & Concorde.Systems.Star_System_Type
              (Location.Destination_System).Name;
         when System_Point =>
            declare
               use Concorde.Vectors;
            begin
               return Concorde.Real_Images.Approximate_Image
                 (abs (Location.Relative_Position
                  / Concorde.Solar_System.Earth_Orbit))
                 & " AU from "
                 & Location.Reference.Identifier;
            end;
         when Orbit =>
            declare
               use Concorde.Vectors;
               Obj : Concorde.Systems.Star_System_Object_Interface'Class
               renames Concorde.Systems.Star_System_Object_Interface'Class
                 (Location.Reference.all);
            begin
               return "orbiting "
                 & Obj.Name
                 & " at"
                 & Natural'Image
                 (Natural
                    ((abs (Location.Apoapsis) - Obj.Radius) / 1000.0))
                 & "km";
            end;
         when In_Community =>
            return "in community on "
              & Concorde.People.Communities.Community_Type (Location.Reference)
              .World.Name;
         when World_Surface =>
            return "on "
              & Concorde.Worlds.World_Type (Location.Reference).Name;
         when On_Ship =>
            return "on "
              & Concorde.Ships.Ship_Type (Location.Reference).Name;
         when At_Installation =>
            return "at "
              & Concorde.Installations.Installation_Type
              (Location.Reference).Facility.Name
              & " on "
              & Current_World (Location).Name;
         when In_Unit =>
            return "in unit";
      end case;
   end Short_Name;

   ---------------------
   -- System_Distance --
   ---------------------

   function System_Distance
     (From, To : Object_Location)
      return Non_Negative_Real
   is
      use Concorde.Vectors;
      pragma Assert (From in System_Point_Location);
      pragma Assert (To in System_Point_Location);
   begin
      return abs (From.Relative_Position - To.Relative_Position);
   end System_Distance;

   ------------------
   -- System_Point --
   ------------------

   function System_Point
     (System            : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Relative_Position : Concorde.Vectors.Vector_3;
      Relative_Velocity : Concorde.Vectors.Vector_3)
      return Object_Location
   is
   begin
      return (System_Point, Concorde.Systems.Star_System_Type (System),
              Relative_Position, Relative_Velocity);
   end System_Point;

   ------------------------------
   -- System_Relative_Position --
   ------------------------------

   function System_Relative_Position
     (Located  : Located_Interface'Class;
      Time     : Concorde.Calendar.Time)
      return Concorde.Vectors.Vector_3
   is
   begin
      return To_System_Point (Located.Location_At (Time), Time)
        .Relative_Position;
   end System_Relative_Position;

   ------------------------------
   -- System_Relative_Position --
   ------------------------------

   function System_Relative_Position
     (Location : Object_Location;
      Time     : Concorde.Calendar.Time)
      return Concorde.Vectors.Vector_3
   is
   begin
      return To_System_Point (Location, Time).Relative_Position;
   end System_Relative_Position;

   ---------------------------
   -- System_Transfer_Orbit --
   ---------------------------

   function System_Transfer_Orbit
     (From_System, To_System : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Object_Location
   is
      use Concorde.Vectors;
      Star : constant Concorde.Stars.Star_Type :=
               Concorde.Stars.Star_Type
                 (From_System.Main_Object);
      Distance : constant Non_Negative_Real :=
                   Star.Mass / Concorde.Solar_System.Solar_Mass
                     * Concorde.Solar_System.Earth_Orbit * 5.0;
      X1       : constant Real := From_System.X;
      X2       : constant Real := To_System.X;
      Y1       : constant Real := From_System.Y;
      Y2       : constant Real := To_System.Y;
      Z1       : constant Real := From_System.Z;
      Z2       : constant Real := To_System.Z;
      V        : constant Vector_3 :=
                   (X1, Y1, Z1) - (X2, Y2, Z2);
      U        : constant Vector_3 := V / abs V;
   begin

      return System_Point
        (System            => From_System,
         Relative_Position => Distance * U,
         Relative_Velocity => U);
   end System_Transfer_Orbit;

   ---------------------
   -- To_System_Point --
   ---------------------

   function To_System_Point
     (Loc  : Object_Location;
      Time : Concorde.Calendar.Time)
      return System_Point_Location
   is
   begin
      case Loc.Loc_Type is
         when Nowhere =>
            raise Constraint_Error
              with "cannot convert nowhere to a system point";
         when Interstellar =>
            raise Constraint_Error
              with "cannot convert " & Short_Name (Loc)
              & " to a system point";
         when System_Point =>
            return Loc;
         when Orbit =>
            declare
               use Concorde.Vectors;
               Primary_Position : constant Object_Location :=
                                    To_System_Point
                                      (Located_Interface'Class
                                         (Loc.Reference.all).Current_Location,
                                       Time);
               Orbit_Position   : constant Concorde.Vectors.Vector_3 :=
                                    Primary_Relative_Position
                                      (Loc, Time);
            begin
               return (System_Point,
                       Located_Interface'Class
                         (Loc.Reference.all).Current_System,
                       Relative_Position =>
                         Primary_Position.Relative_Position
                           + Orbit_Position,
                       Relative_Velocity =>
                         Primary_Position.Relative_Velocity);
            end;
         when World_Surface | In_Community
            | On_Ship | At_Installation | In_Unit =>
            return To_System_Point
              (Located_Interface'Class
                 (Loc.Reference.all).Current_Location,
               Time);
      end case;
   end To_System_Point;

   ------------------
   -- World_Sector --
   ------------------

   function World_Sector
     (Location : Object_Location)
      return Positive
   is
   begin
      if Location.Loc_Type = World_Surface then
         return Location.Sector;
      else
         return World_Sector
           (Located_Interface'Class (Location.Reference.all).Current_Location);
      end if;
   end World_Sector;

   -------------------
   -- World_Surface --
   -------------------

   function World_Surface
     (World  : not null access constant
        Concorde.Objects.Root_Object_Type'Class;
      Sector : Positive)
      return Object_Location
   is
   begin
      return (World_Surface, World, Sector);
   end World_Surface;

end Concorde.Locations;
