with Concorde.Calendar;
with Concorde.Systems;

package Concorde.Ships.Navigation is

   function Journey_Time
     (Ship        : Concorde.Ships.Ship_Type;
      Start_Time  : Concorde.Calendar.Time;
      Destination : Concorde.Locations.Object_Location)
      return Duration
     with Pre => Ship.Current_System.Index
       = Concorde.Locations.Current_System (Destination).Index;

--     function Jump_Time
--       (Ship        : Concorde.Ships.Ship_Type;
--        Start_Time  : Concorde.Calendar.Time;
--        Destination : Concorde.Systems.Star_System_Type)
--        return Duration;

end Concorde.Ships.Navigation;
