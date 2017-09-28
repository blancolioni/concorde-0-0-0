package body Concorde.Ships.Navigation is

   ------------------
   -- Journey_Time --
   ------------------

   function Journey_Time
     (Ship        : Concorde.Ships.Ship_Type;
      Start_Time  : Concorde.Dates.Date_Type;
      Destination : Concorde.Locations.Object_Location)
      return Duration
   is
      use Concorde.Locations;
      From : constant System_Point_Location :=
               To_System_Point (Ship.Location_At (Start_Time));
      To   : constant System_Point_Location :=
               To_System_Point (Destination);
      D    : constant Non_Negative_Real :=
               System_Distance (From, To);
   begin
      return Duration (D / 5_000_000.0);
   end Journey_Time;

end Concorde.Ships.Navigation;
