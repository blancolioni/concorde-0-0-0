private with Concorde.Events;

with Concorde.Corporations;

package Concorde.Managers.Corporations is

   type Root_Corporation_Manager is
     new Root_Manager_Type with private;

   procedure Create
     (Manager : not null access Root_Corporation_Manager'Class;
      Corporation     : Concorde.Corporations.Corporation_Type);

   type Corporation_Manager is access all Root_Corporation_Manager'Class;

   function Create_Manager
     (Corporation : Concorde.Corporations.Corporation_Type)
      return Corporation_Manager;

private

   type Root_Corporation_Manager is
     new Root_Manager_Type with
      record
         Corporation : Concorde.Corporations.Corporation_Type;
      end record;

   overriding function Description
     (Manager : Root_Corporation_Manager)
      return String
   is (Manager.Corporation.Identifier & " manager");

   overriding procedure On_Activated
     (Manager : in out Root_Corporation_Manager);

   type Root_Corporation_Event_Handler is
     abstract new Concorde.Objects.Object_Handler_Interface with
      record
         Manager : Corporation_Manager;
      end record;

   overriding procedure Handle
     (Handler : in out Root_Corporation_Event_Handler;
      Event   : Concorde.Events.Root_Event_Type'Class;
      Object  : not null access constant
        Concorde.Objects.Root_Object_Type'Class);

   procedure Handle_Corporation_Event
     (Handler : in out Root_Corporation_Event_Handler;
      Corporation     : Concorde.Corporations.Corporation_Type)
   is abstract;

end Concorde.Managers.Corporations;
