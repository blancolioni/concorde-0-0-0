with Ada.Containers.Indefinite_Holders;

with WL.Heaps;

with Concorde.Signals.Standard;

package body Concorde.Objects.Queues is

   package Signal_Holders is
     new Ada.Containers.Indefinite_Holders
       (Concorde.Signals.Signal_Type, Concorde.Signals."=");

   package Event_Holders is
     new Ada.Containers.Indefinite_Holders
       (Concorde.Events.Root_Event_Type'Class,
        Concorde.Events."=");

   type Queue_Element is
      record
         Object : Object_Type;
         Signal : Signal_Holders.Holder;
         Event  : Event_Holders.Holder;
      end record;

   package Object_Queues is
     new WL.Heaps (Concorde.Dates.Date_Type, Queue_Element,
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
      Next_Event (Object, Date,
                  Concorde.Signals.Standard.Object_Activated,
                  Concorde.Events.Null_Event (Date));
   end Next_Event;

   ----------------
   -- Next_Event --
   ----------------

   procedure Next_Event
     (Object : not null access constant Root_Object_Type'Class;
      Date   : Concorde.Dates.Date_Type;
      Signal : Concorde.Signals.Signal_Type;
      Event  : Concorde.Events.Root_Event_Type'Class)
   is
   begin
      Queue.Insert
        (Key     => Date,
         Element =>
           Queue_Element'
             (Object => Object_Type (Object),
              Signal => Signal_Holders.To_Holder (Signal),
              Event  => Event_Holders.To_Holder (Event)));
   end Next_Event;

   ----------------
   -- Scan_Queue --
   ----------------

   procedure Scan_Queue is
      use Concorde.Dates;
      Now : constant Date_Type := Concorde.Dates.Current_Date;
   begin
      while not Queue.Is_Empty
        and then Queue.Maximum_Key <= Now
      loop
         declare
            Element    : constant Queue_Element :=
                           Queue.Maximum_Element;
            Signal     : constant Concorde.Signals.Signal_Type :=
                           Element.Signal.Element;
            Event      : Concorde.Events.Root_Event_Type'Class :=
                           Element.Event.Element;
         begin
            Event.Set_Time_Stamp (Queue.Maximum_Key);
            Queue.Delete_Maximum;
            Element.Object.Signal (Signal, Event);
         end;
      end loop;
   end Scan_Queue;

end Concorde.Objects.Queues;
