private with Concorde.Events;
private with Concorde.Government;

with Concorde.People.Communities;

package Concorde.Managers.Communities is

   type Root_Community_Manager is
     new Root_Manager_Type with private;

   procedure Create
     (Manager   : not null access Root_Community_Manager'Class;
      Community : Concorde.People.Communities.Community_Type);

   type Community_Manager is access all Root_Community_Manager'Class;

   function Create_Manager
     (Community : Concorde.People.Communities.Community_Type)
      return Community_Manager;

private

   type Root_Community_Manager is
     new Root_Manager_Type with
      record
         Community  : Concorde.People.Communities.Community_Type;
      end record;

   overriding function Description
     (Manager : Root_Community_Manager)
      return String
   is (Manager.Community.Identifier & " manager");

   overriding procedure On_Activated
     (Manager : in out Root_Community_Manager);

   type Root_Community_Event_Handler is
     abstract new Concorde.Objects.Object_Handler_Interface with
      record
         Manager : Community_Manager;
      end record;

   overriding procedure Handle
     (Handler : in out Root_Community_Event_Handler;
      Event   : Concorde.Events.Root_Event_Type'Class;
      Object  : not null access constant
        Concorde.Objects.Root_Object_Type'Class);

   procedure Handle_Community_Event
     (Handler : in out Root_Community_Event_Handler;
      Community     : Concorde.People.Communities.Community_Type)
   is abstract;

end Concorde.Managers.Communities;
