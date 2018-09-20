private with Concorde.Events;

with Concorde.Industries;

package Concorde.Managers.Industries is

   type Root_Industry_Manager is
     new Root_Manager_Type with private;

   procedure Create
     (Manager : not null access Root_Industry_Manager'Class;
      Industry     : Concorde.Industries.Industry_Type);

   type Industry_Manager is access all Root_Industry_Manager'Class;

   function Create_Manager
     (Industry : Concorde.Industries.Industry_Type)
      return Industry_Manager;

private

   type Root_Industry_Manager is
     new Root_Manager_Type with
      record
         Industry : Concorde.Industries.Industry_Type;
      end record;

   overriding function Description
     (Manager : Root_Industry_Manager)
      return String
   is (Manager.Industry.Identifier & " manager");

   overriding procedure On_Activated
     (Manager : in out Root_Industry_Manager);

   type Root_Industry_Event_Handler is
     abstract new Concorde.Objects.Object_Handler_Interface with
      record
         Manager : Industry_Manager;
      end record;

   overriding procedure Handle
     (Handler : in out Root_Industry_Event_Handler;
      Event   : Concorde.Events.Root_Event_Type'Class;
      Object  : not null access constant
        Concorde.Objects.Root_Object_Type'Class);

   procedure Handle_Industry_Event
     (Handler : in out Root_Industry_Event_Handler;
      Industry     : Concorde.Industries.Industry_Type)
   is abstract;

end Concorde.Managers.Industries;
