with Concorde.Dates;
with Concorde.Objects.Queues;

package body Concorde.Updates is

   -------------
   -- Advance --
   -------------

   procedure Advance (Interval : Duration) is
   begin
      Concorde.Dates.Tick (Interval);
      Concorde.Objects.Queues.Scan_Queue;
   end Advance;

end Concorde.Updates;
