with Concorde.Calendar;

package Concorde.Ships.Navigation is

   function Journey_Time
     (Ship        : not null access constant Root_Ship_Type'Class;
      Departure   : Concorde.Calendar.Time;
      Destination : Concorde.Locations.Object_Location)
      return Duration;

end Concorde.Ships.Navigation;
