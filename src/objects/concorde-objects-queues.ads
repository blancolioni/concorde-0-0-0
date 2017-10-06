with Concorde.Calendar;

package Concorde.Objects.Queues is

   procedure Next_Event
     (Object : not null access constant Root_Object_Type'Class;
      Date   : Concorde.Calendar.Time);

   procedure Next_Event
     (Object : not null access constant Root_Object_Type'Class;
      Date   : Concorde.Calendar.Time;
      Signal : Concorde.Signals.Signal_Type;
      Event  : Concorde.Events.Root_Event_Type'Class);

   procedure Next_Event
     (Object : not null access constant Root_Object_Type'Class;
      Current_Date : Concorde.Calendar.Time;
      Delay_Days   : Positive);

   procedure Next_Event
     (Object : not null access constant Root_Object_Type'Class;
      Current_Date : Concorde.Calendar.Time;
      Delay_Days   : Positive;
      Signal       : Concorde.Signals.Signal_Type;
      Event  : Concorde.Events.Root_Event_Type'Class);

   procedure Scan_Queue;

end Concorde.Objects.Queues;
