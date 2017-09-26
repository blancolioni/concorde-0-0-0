with Concorde.Dates;

package Concorde.Objects.Queues is

   procedure Next_Event
     (Object : not null access constant Root_Object_Type'Class;
      Date   : Concorde.Dates.Date_Type);

   procedure Scan_Queue;

end Concorde.Objects.Queues;
