with Concorde.Events;

package Concorde.Systems.Events is

   type System_Event_Handler is
     abstract new Concorde.Objects.Object_Handler_Interface with private;

   overriding procedure Handle
     (Handler : in out System_Event_Handler;
      Event   : Concorde.Events.Root_Event_Type'Class;
      Object  : not null access constant
        Concorde.Objects.Root_Object_Type'Class);

   procedure On_Event
     (Handler : System_Event_Handler;
      Event   : Concorde.Events.Root_Event_Type'Class;
      System  : not null access constant Root_Star_System_Type'Class)
   is abstract;

   type Ship_System_Event_Handler is
     abstract new System_Event_Handler with private;

   type Ship_System_Handler_Access is
     access all Ship_System_Event_Handler'Class;

   overriding procedure On_Event
     (Handler : Ship_System_Event_Handler;
      Event   : Concorde.Events.Root_Event_Type'Class;
      System  : not null access constant Root_Star_System_Type'Class);

   procedure On_Ship_Event
     (Handler : Ship_System_Event_Handler;
      System  : not null access constant Root_Star_System_Type'Class;
      Ship    : not null access constant
        Concorde.Ships.Root_Ship_Type'Class)
   is abstract;

   procedure Add_Ship_Handler
     (System : Star_System_Type;
      Signal : Concorde.Signals.Signal_Type;
      Handler : not null access Ship_System_Event_Handler'Class);

private

   type System_Event_Handler is
     abstract new Concorde.Objects.Object_Handler_Interface with
      record
         System : Star_System_Type;
      end record;

   type Ship_System_Event_Handler is
     abstract new System_Event_Handler with null record;

end Concorde.Systems.Events;
