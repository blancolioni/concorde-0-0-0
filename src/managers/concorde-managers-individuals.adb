with Concorde.Objects.Queues;
with Concorde.Signals.Standard;

with Concorde.Commodities;
with Concorde.Facilities;
with Concorde.Trades;

with Concorde.Powers.Execution;
with Concorde.People.Individuals.Work;

package body Concorde.Managers.Individuals is

   ------------
   -- Create --
   ------------

   procedure Create
     (Manager : not null access Root_Individual_Manager'Class;
      Individual     : Concorde.People.Individuals.Individual_Type)
   is
   begin
      Manager.Object := Individual;
      Manager.Individual := Individual;
      Manager.Government :=
        Concorde.Government.Government_Type
          (Individual.Government);
      Individual.Update.Add_Handler
        (Concorde.Signals.Standard.Object_Activated,
         new Object_Activated_Handler'(Manager => Manager_Type (Manager)));
   end Create;

   --------------------
   -- Create_Manager --
   --------------------

   function Create_Manager
     (Individual : Concorde.People.Individuals.Individual_Type)
      return Manager_Type
   is
   begin
      return Manager : constant Manager_Type := new Root_Individual_Manager do
         Individual_Manager (Manager).Create (Individual);
      end return;
   end Create_Manager;

   ------------
   -- Handle --
   ------------

   overriding procedure Handle
     (Handler : in out Root_Individual_Event_Handler;
      Event   : Concorde.Events.Root_Event_Type'Class;
      Object  : not null access constant
        Concorde.Objects.Root_Object_Type'Class)
   is
      pragma Unreferenced (Object);
   begin
      Handler.Manager.Time := Event.Time_Stamp;
      Handler.Manager.On_Activated;
   end Handle;

   ------------------
   -- On_Activated --
   ------------------

   overriding procedure On_Activated
     (Manager : in out Root_Individual_Manager)
   is
      use type Concorde.Calendar.Time;
      use type Concorde.Work.Work_Item;
   begin
      if Manager.Current_Work /= null then
         Concorde.People.Individuals.Work.Perform_Work
           (Manager.Individual, Manager.Current_Work);
         Manager.Current_Work := null;
      end if;

      if not Manager.Work_Queue.Is_Empty then
         Manager.Individual.Log
           ("next work item: " & Manager.Work_Queue.Maximum_Element.Show);
         Manager.Current_Work :=
           Manager.Work_Queue.Maximum_Element;
         Manager.Work_Queue.Delete_Maximum;
         Concorde.Objects.Queues.Next_Event
           (Object => Manager.Individual,
            Date   =>
              Concorde.Calendar.Clock
            + Concorde.Powers.Execution.Execution_Work
              (Manager.Current_Work.Power,
               Manager.Current_Work.Target));
      else
         Concorde.Objects.Queues.Next_Event
           (Manager.Individual, Manager.Time, Delay_Days => 1);
      end if;
   end On_Activated;

end Concorde.Managers.Individuals;
