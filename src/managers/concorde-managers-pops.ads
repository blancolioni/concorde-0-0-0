private with Concorde.Events;
private with Concorde.Government;

with Concorde.People.Pops;

package Concorde.Managers.Pops is

   type Root_Pop_Manager is
     new Root_Manager_Type with private;

   procedure Create
     (Manager : not null access Root_Pop_Manager'Class;
      Pop     : Concorde.People.Pops.Pop_Type);

   type Pop_Manager is access all Root_Pop_Manager'Class;

   function Create_Manager
     (Pop : Concorde.People.Pops.Pop_Type)
      return Pop_Manager;

private

   type Root_Pop_Manager is
     new Root_Manager_Type with
      record
         Pop        : Concorde.People.Pops.Pop_Type;
         Government : Concorde.Government.Government_Type;
         Happiness  : Unit_Real := 1.0;
      end record;

   overriding function Description
     (Manager : Root_Pop_Manager)
      return String
   is (Manager.Pop.Identifier & " manager");

   overriding procedure On_Activated
     (Manager : in out Root_Pop_Manager);

   type Root_Pop_Event_Handler is
     abstract new Concorde.Objects.Object_Handler_Interface with
      record
         Manager : Pop_Manager;
      end record;

   overriding procedure Handle
     (Handler : in out Root_Pop_Event_Handler;
      Event   : Concorde.Events.Root_Event_Type'Class;
      Object  : not null access constant
        Concorde.Objects.Root_Object_Type'Class);

   procedure Handle_Pop_Event
     (Handler : in out Root_Pop_Event_Handler;
      Pop     : Concorde.People.Pops.Pop_Type)
   is abstract;

end Concorde.Managers.Pops;
