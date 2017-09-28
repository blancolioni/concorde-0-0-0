with Concorde.Dates;
with Concorde.Events;
with Concorde.Objects;

package Concorde.Managers is

   type Root_Manager_Type is abstract tagged private;

   procedure Deactivate (Manager : not null access Root_Manager_Type'Class);
   procedure Activate (Manager : not null access Root_Manager_Type'Class);

   procedure On_Activated
     (Manager : in out Root_Manager_Type)
   is null;

   type Manager_Type is access all Root_Manager_Type'Class;

private

   type Root_Manager_Type is abstract tagged
      record
         Active : Boolean := True;
         Object : access constant Concorde.Objects.Root_Object_Type'Class;
         Time   : Concorde.Dates.Date_Type;
      end record;

   type Object_Activated_Handler is
     new Concorde.Objects.Object_Handler_Interface with
      record
         Manager : Manager_Type;
      end record;

   overriding procedure Handle
     (Handler : in out Object_Activated_Handler;
      Event   : Concorde.Events.Root_Event_Type'Class;
      Object  : not null access constant
        Concorde.Objects.Root_Object_Type'Class);

end Concorde.Managers;
