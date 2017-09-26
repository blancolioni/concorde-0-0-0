with WL.Heaps;

with Concorde.Signals.Standard;

package body Concorde.Objects.Queues is

   package Object_Queues is
     new WL.Heaps (Concorde.Dates.Date_Type, Object_Type,
                   "<" => Concorde.Dates.">");

   Queue : Object_Queues.Heap;

   ----------------
   -- Next_Event --
   ----------------

   procedure Next_Event
     (Object : not null access constant Root_Object_Type'Class;
      Date   : Concorde.Dates.Date_Type)
   is
   begin
      Queue.Insert (Date, Object_Type (Object));
   end Next_Event;

   ----------------
   -- Scan_Queue --
   ----------------

   procedure Scan_Queue is
      use Concorde.Dates;
      Now : constant Date_Type := Concorde.Dates.Current_Date;
   begin
      while not Queue.Is_Empty
        and then Queue.Maximum_Key >= Now
      loop
         declare
            Object : constant Object_Type :=
                       Queue.Maximum_Element;
         begin
            Queue.Delete_Maximum;
            Object.Signal (Concorde.Signals.Standard.Object_Activated);
         end;
      end loop;
   end Scan_Queue;

end Concorde.Objects.Queues;
