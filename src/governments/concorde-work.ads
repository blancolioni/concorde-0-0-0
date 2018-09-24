with Concorde.Calendar;
with Concorde.Objects;
with Concorde.Powers;

package Concorde.Work is

   type Work_Priority is private;

   function Higher (Left, Right : Work_Priority) return Boolean;

   type Root_Work_Item is abstract tagged private;

   function Show (Item : Root_Work_Item) return String is abstract;
   function Power (Item : Root_Work_Item) return Concorde.Powers.Power_Type
                   is abstract;

   function Priority (Item : Root_Work_Item) return Work_Priority;
   function Start (Item : Root_Work_Item) return Concorde.Calendar.Time;
   function Deadline (Item : Root_Work_Item) return Concorde.Calendar.Time;
   function Target (Item : Root_Work_Item) return Concorde.Objects.Object_Type;

   type Work_Item is access constant Root_Work_Item'Class;

private

   type Work_Priority is new Positive;

   function Higher (Left, Right : Work_Priority) return Boolean
                    renames ">";

   type Root_Work_Item is abstract tagged
      record
         Priority : Work_Priority           := 1;
         Start    : Concorde.Calendar.Time  := Concorde.Calendar.Start;
         Deadline : Concorde.Calendar.Time  := Concorde.Calendar.Start;
         Target   : Concorde.Objects.Object_Type := null;
      end record;

   function Priority (Item : Root_Work_Item) return Work_Priority
   is (Item.Priority);

   function Start (Item : Root_Work_Item) return Concorde.Calendar.Time
   is (Item.Start);

   function Deadline (Item : Root_Work_Item) return Concorde.Calendar.Time
   is (Item.Deadline);

   function Target (Item : Root_Work_Item) return Concorde.Objects.Object_Type
   is (Item.Target);

end Concorde.Work;
