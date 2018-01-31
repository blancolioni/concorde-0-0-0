private with Concorde.Events;

with Concorde.Ministries;

package Concorde.Managers.Ministries is

   type Root_Ministry_Manager is
     new Root_Manager_Type with private;

   procedure Create
     (Manager : not null access Root_Ministry_Manager'Class;
      Ministry     : Concorde.Ministries.Ministry_Type);

   type Ministry_Manager is access all Root_Ministry_Manager'Class;

   function Create_Manager
     (Ministry : Concorde.Ministries.Ministry_Type)
      return Ministry_Manager;

private

   type Root_Ministry_Manager is
     new Root_Manager_Type with
      record
         Ministry : Concorde.Ministries.Ministry_Type;
      end record;

   overriding procedure On_Activated
     (Manager : in out Root_Ministry_Manager);

   type Root_Ministry_Event_Handler is
     abstract new Concorde.Objects.Object_Handler_Interface with
      record
         Manager : Ministry_Manager;
      end record;

   overriding procedure Handle
     (Handler : in out Root_Ministry_Event_Handler;
      Event   : Concorde.Events.Root_Event_Type'Class;
      Object  : not null access constant
        Concorde.Objects.Root_Object_Type'Class);

   procedure Handle_Ministry_Event
     (Handler : in out Root_Ministry_Event_Handler;
      Ministry     : Concorde.Ministries.Ministry_Type)
   is abstract;

end Concorde.Managers.Ministries;
