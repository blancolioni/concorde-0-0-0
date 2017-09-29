with Concorde.Dates;

package Concorde.Objects.Queues is

   procedure Next_Event
     (Object : not null access constant Root_Object_Type'Class;
      Date   : Concorde.Dates.Date_Type);

   procedure Next_Event
     (Object : not null access constant Root_Object_Type'Class;
      Date   : Concorde.Dates.Date_Type;
      Signal : Concorde.Signals.Signal_Type;
      Event  : Concorde.Events.Root_Event_Type'Class);

   procedure Scan_Queue;

end Concorde.Objects.Queues;
