with Concorde.Calendar;

package Concorde.Events is

   type Root_Event_Type is abstract tagged private;

   function Time_Stamp
     (Event : Root_Event_Type'Class)
      return Concorde.Calendar.Time;

   procedure Set_Time_Stamp
     (Event      : in out Root_Event_Type'Class;
      Time_Stamp : Concorde.Calendar.Time);

   function Null_Event
     (Time_Stamp : Concorde.Calendar.Time)
     return Root_Event_Type'Class;

private

   type Root_Event_Type is abstract tagged
      record
         Time_Stamp : Concorde.Calendar.Time;
      end record;

   function Time_Stamp
     (Event : Root_Event_Type'Class)
      return Concorde.Calendar.Time
   is (Event.Time_Stamp);

   type Null_Event_Type is new Root_Event_Type with null record;

   function Null_Event
     (Time_Stamp : Concorde.Calendar.Time)
      return Root_Event_Type'Class
   is (Null_Event_Type'(Time_Stamp => Time_Stamp));

end Concorde.Events;
