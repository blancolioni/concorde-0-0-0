private with Concorde.Events;
private with Concorde.Government;

with Concorde.People.Individuals;

package Concorde.Managers.Individuals is

   type Root_Individual_Manager is
     new Root_Manager_Type with private;

   procedure Create
     (Manager : not null access Root_Individual_Manager'Class;
      Individual     : Concorde.People.Individuals.Individual_Type);

   type Individual_Manager is access all Root_Individual_Manager'Class;

   function Create_Manager
     (Individual : Concorde.People.Individuals.Individual_Type)
      return Manager_Type;

private

   type Root_Individual_Manager is
     new Root_Manager_Type with
      record
         Individual : Concorde.People.Individuals.Individual_Type;
         Government : Concorde.Government.Government_Type;
         Started    : Boolean := False;
         Day_Start  : Concorde.Calendar.Time;
      end record;

   overriding function Description
     (Manager : Root_Individual_Manager)
      return String
   is (Manager.Individual.Full_Name & " manager");

   overriding procedure On_Activated
     (Manager : in out Root_Individual_Manager);

   type Root_Individual_Event_Handler is
     abstract new Concorde.Objects.Object_Handler_Interface with
      record
         Manager : Individual_Manager;
      end record;

   overriding procedure Handle
     (Handler : in out Root_Individual_Event_Handler;
      Event   : Concorde.Events.Root_Event_Type'Class;
      Object  : not null access constant
        Concorde.Objects.Root_Object_Type'Class);

   procedure Handle_Individual_Event
     (Handler : in out Root_Individual_Event_Handler;
      Individual     : Concorde.People.Individuals.Individual_Type)
   is abstract;

end Concorde.Managers.Individuals;
