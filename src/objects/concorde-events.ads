package Concorde.Events is

   type Root_Event_Type is abstract tagged private;

   function Null_Event return Root_Event_Type'Class;

private

   type Root_Event_Type is abstract tagged null record;

   type Null_Event_Type is new Root_Event_Type with null record;

   function Null_Event return Root_Event_Type'Class
   is (Null_Event_Type'(null record));

end Concorde.Events;
