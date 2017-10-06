with Concorde.Calendar;
with Concorde.Objects.Queues;

package body Concorde.Updates is

   -------------
   -- Advance --
   -------------

   procedure Advance (Interval : Duration) is
   begin
      Concorde.Calendar.Advance (Interval);
      Concorde.Objects.Queues.Scan_Queue;
   end Advance;

end Concorde.Updates;
