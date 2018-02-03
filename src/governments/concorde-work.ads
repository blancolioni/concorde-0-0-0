with Concorde.Calendar;

package Concorde.Work is

   type Work_Priority is private;

   function Higher (Left, Right : Work_Priority) return Boolean;

   type Root_Work_Item is abstract tagged private;

   function Show (Item : Root_Work_Item) return String is abstract;
   function Cost (Item : Root_Work_Item) return Duration is abstract;

   function Priority (Item : Root_Work_Item) return Work_Priority;
   function Deadline (Item : Root_Work_Item) return Concorde.Calendar.Time;

   type Work_Item is access constant Root_Work_Item'Class;

private

   type Work_Priority is new Positive;

   function Higher (Left, Right : Work_Priority) return Boolean
                    renames ">";

   type Root_Work_Item is abstract tagged
      record
         Priority : Work_Priority           := 1;
         Deadline : Concorde.Calendar.Time  := Concorde.Calendar.Start;
      end record;

   function Priority (Item : Root_Work_Item) return Work_Priority
   is (Item.Priority);

   function Deadline (Item : Root_Work_Item) return Concorde.Calendar.Time
   is (Item.Deadline);

end Concorde.Work;
