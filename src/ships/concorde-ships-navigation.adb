with Xi.Float_Images;

with Concorde.Solar_System;

with Concorde.Elementary_Functions;

package body Concorde.Ships.Navigation is

   ------------------
   -- Journey_Time --
   ------------------

   function Journey_Time
     (Ship        : Concorde.Ships.Ship_Type;
      Start_Time  : Concorde.Calendar.Time;
      Destination : Concorde.Locations.Object_Location)
      return Duration
   is
      use Concorde.Elementary_Functions;
      use Concorde.Locations;
      From : constant System_Point_Location :=
               To_System_Point (Ship.Location_At (Start_Time));
      To   : constant System_Point_Location :=
               To_System_Point (Destination);
      D    : constant Non_Negative_Real :=
               System_Distance (From, To);
      F    : constant Non_Negative_Real := Ship.Maximum_Thrust;
      A    : constant Non_Negative_Real :=
               Non_Negative_Real'Min
                 (F / Ship.Current_Mass,
                  Concorde.Solar_System.Earth_Gravity);
      T    : constant Non_Negative_Real :=
               2.0 * Sqrt (D / A);
   begin
      Ship.Log_Movement
        ("distance "
         & Xi.Float_Images.Image
           (D / Concorde.Solar_System.Earth_Orbit)
         & " AU; accel "
         & Xi.Float_Images.Image (A / 9.81)
         & "g; travel time "
         & Xi.Float_Images.Image (T / 3600.0) & " hours");
      return Duration (T);
   end Journey_Time;

end Concorde.Ships.Navigation;
