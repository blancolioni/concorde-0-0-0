with Ada.Numerics;

with Concorde.Constants;
with Concorde.Solar_System;
with Concorde.Elementary_Functions;
with Concorde.Random;

with Concorde.Installations;
with Concorde.Ships;
with Concorde.Stars;
with Concorde.Systems;
with Concorde.Worlds;

package body Concorde.Locations is

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
         when Orbit | World_Surface =>
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
   end Current_System;

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

   ---------------
   -- Long_Name --
   ---------------

   function Long_Name (Location : Object_Location) return String is
   begin
      return Short_Name (Location);
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
      Position       : Newton.Vector_3;
      Velocity       : Newton.Vector_3)
      return Object_Location
   is
      pragma Unreferenced (Velocity);
      use Newton.Matrices;
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
              Concorde.Dates.Current_Date,
              Concorde.Random.Unit_Random, Duration (Period), True);
   end Orbit;

   -----------
   -- Orbit --
   -----------

   function Orbit
     (Primary        : not null access constant
        Concorde.Objects.Massive_Object_Interface'Class;
      Altitude       : Real)
      return Object_Location
   is
      use Concorde.Elementary_Functions;
      World   : constant Concorde.Worlds.World_Type :=
                  Concorde.Worlds.World_Type (Primary);
      R       : constant Non_Negative_Real :=
                  World.Radius + Altitude;
      V       : constant Non_Negative_Real :=
                  2.0 * Ada.Numerics.Pi * R
                    / World.Day_Length;
   begin
      return Orbit (Primary,
                    Position => (R, 0.0, 0.0),
                    Velocity => (0.0, V, 0.0));
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
     (Location : Object_Location)
      return Newton.Vector_3
   is
      use Newton.Matrices;
      use Concorde.Dates;
      use Concorde.Geometry;
      R            : constant Non_Negative_Real := abs (Location.Apoapsis);
      Start        : constant Non_Negative_Real :=
                       Location.Start_Offset;
      Elapsed      : constant Duration :=
                       Concorde.Dates.Current_Date - Location.Start_Time;
      Total_Orbits : constant Real :=
                       Start + Real (Elapsed) / Real (Location.Period);
      Theta        : constant Radians :=
                       Degrees_To_Radians (Total_Orbits * 360.0);
   begin
      return (R * Cos (Theta), 0.0, R * Sin (Theta));
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
         when Orbit =>
            return "orbiting "
              & Concorde.Systems.Star_System_Object_Interface'Class
              (Location.Reference.all).Name;
         when World_Surface =>
            return "on "
              & Concorde.Worlds.World_Type (Location.Reference).Name;
         when On_Ship =>
            return "on "
              & Concorde.Ships.Ship_Type
              (Location.Reference).Short_Description;
         when At_Installation =>
            return "at "
              & Concorde.Installations.Installation_Type
              (Location.Reference).Facility.Name;
         when In_Unit =>
            return "in unit";
      end case;
   end Short_Name;

   ---------------------------
   -- System_Transfer_Orbit --
   ---------------------------

   function System_Transfer_Orbit
     (System : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Object_Location
   is
      Star : constant Concorde.Stars.Star_Type :=
               Concorde.Stars.Star_Type
                 (System.Main_Object);
      Distance : constant Non_Negative_Real :=
                   Star.Mass / Concorde.Solar_System.Earth_Mass
                     * Concorde.Solar_System.Earth_Orbit * 10.0;
   begin
      return Orbit
        (Primary  => Star,
         Position => (Distance, 0.0, 0.0),
         Velocity => (1.0, 0.0, 0.0));
   end System_Transfer_Orbit;

   ------------------
   -- World_Sector --
   ------------------

   function World_Sector
     (Location : Object_Location)
      return Positive
   is
   begin
      return Location.Sector;
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
