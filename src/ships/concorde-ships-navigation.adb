with Xi.Float_Images;

with Concorde.Elementary_Functions;
with Concorde.Solar_System;

package body Concorde.Ships.Navigation is

   ------------------
   -- Journey_Time --
   ------------------

   function Journey_Time
     (Ship        : not null access constant Root_Ship_Type'Class;
      Departure   : Concorde.Calendar.Time;
      Destination : Concorde.Locations.Object_Location)
      return Duration
   is
      use Concorde.Elementary_Functions;
      use Concorde.Locations;
      From : constant System_Point_Location :=
               To_System_Point (Ship.Location_At (Departure), Departure);
      To   : constant System_Point_Location :=
               To_System_Point (Destination, Departure);
      D    : constant Non_Negative_Real :=
               System_Distance (From, To);
      F    : constant Non_Negative_Real := Ship.Maximum_Thrust;
      A    : constant Non_Negative_Real :=
               Non_Negative_Real'Min
                 (F / Ship.Current_Mass,
                  2.0 * Concorde.Solar_System.Earth_Gravity);
      T    : constant Non_Negative_Real :=
               2.0 * Sqrt (D / A);
   begin
      Ship.Log_Movement
        (Long_Name (From) & " to " & Long_Name (To)
         & ": distance "
         & Xi.Float_Images.Image
           (D / Concorde.Solar_System.Earth_Orbit)
         & " AU"
         & "; cargo mass: "
         & Xi.Float_Images.Image (Ship.Total_Mass / 1000.0)
         & "t"
         & "; total mass: "
         & Xi.Float_Images.Image (Ship.Current_Mass / 1000.0)
         & "t"
         & "; accel "
         & Xi.Float_Images.Image (A / 9.81)
         & "g"
         & "; travel time "
         & Xi.Float_Images.Image (T / 3600.0) & " hours");
      return Duration (T);
   end Journey_Time;

end Concorde.Ships.Navigation;
