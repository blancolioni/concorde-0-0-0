private with Concorde.Events;

with Concorde.Installations;

package Concorde.Managers.Installations is

   type Root_Installation_Manager is
     new Root_Manager_Type with private;

   procedure Create
     (Manager : not null access Root_Installation_Manager'Class;
      Installation     : Concorde.Installations.Installation_Type);

   type Installation_Manager is access all Root_Installation_Manager'Class;

   function Create_Manager
     (Installation : Concorde.Installations.Installation_Type)
      return Installation_Manager;

private

   type Root_Installation_Manager is
     new Root_Manager_Type with
      record
         Installation : Concorde.Installations.Installation_Type;
      end record;

   overriding procedure On_Activated
     (Manager : in out Root_Installation_Manager);

   type Root_Installation_Event_Handler is
     abstract new Concorde.Objects.Object_Handler_Interface with
      record
         Manager : Installation_Manager;
      end record;

   overriding procedure Handle
     (Handler : in out Root_Installation_Event_Handler;
      Event   : Concorde.Events.Root_Event_Type'Class;
      Object  : not null access constant
        Concorde.Objects.Root_Object_Type'Class);

   procedure Handle_Installation_Event
     (Handler : in out Root_Installation_Event_Handler;
      Installation     : Concorde.Installations.Installation_Type)
   is abstract;

end Concorde.Managers.Installations;
