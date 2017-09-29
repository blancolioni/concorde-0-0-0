package body Concorde.Events is

   --------------------
   -- Set_Time_Stamp --
   --------------------

   procedure Set_Time_Stamp
     (Event      : in out Root_Event_Type'Class;
      Time_Stamp : Concorde.Dates.Date_Type)
   is
   begin
      Event.Time_Stamp := Time_Stamp;
   end Set_Time_Stamp;

end Concorde.Events;
