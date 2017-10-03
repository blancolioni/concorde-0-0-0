package body Concorde.Systems.Events is

   ----------------------
   -- Add_Ship_Handler --
   ----------------------

   procedure Add_Ship_Handler
     (System  : Star_System_Type;
      Signal  : Concorde.Signals.Signal_Type;
      Handler : not null access Ship_System_Event_Handler'Class)
   is
   begin
      Handler.System := System;
      System.Update.Add_Handler
        (Signal, Handler);
   end Add_Ship_Handler;

   ------------
   -- Handle --
   ------------

   overriding procedure Handle
     (Handler : in out System_Event_Handler;
      Event   : Concorde.Events.Root_Event_Type'Class;
      Object  : not null access constant
        Concorde.Objects.Root_Object_Type'Class)
   is
   begin
      System_Event_Handler'Class (Handler).On_Event
        (Event, Root_Star_System_Type (Object.all)'Access);
   end Handle;

   --------------
   -- On_Event --
   --------------

   overriding procedure On_Event
     (Handler : Ship_System_Event_Handler;
      Event   : Concorde.Events.Root_Event_Type'Class;
      System  : not null access constant Root_Star_System_Type'Class)
   is
   begin
      Ship_System_Event_Handler'Class (Handler).On_Ship_Event
        (System, Ship_Event (Event).Ship);
   end On_Event;

end Concorde.Systems.Events;
