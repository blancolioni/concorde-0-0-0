with Concorde.Objects.Queues;
with Concorde.Signals.Standard;

with Concorde.Bureaucracy;
with Concorde.Commodities;
with Concorde.Facilities;
with Concorde.Ministries;
with Concorde.Trades;

with Concorde.Powers;
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
         Manager.Individual.Log
           ("executing: " & Manager.Current_Work.Show);
         Concorde.People.Individuals.Work.Perform_Work
           (Manager.Individual, Manager.Current_Work);
         Manager.Current_Work := null;
      end if;

      if Manager.Individual.Has_Office then
         declare
            use Concorde.Ministries;
            Remaining_Work : Work_Item_Queue.Heap;
            Ministry       : constant Concorde.Ministries.Ministry_Type :=
                               Ministry_Type
                                 (Manager.Individual.Office);
         begin
            while not Manager.Work_Queue.Is_Empty loop
               declare
                  Priority : constant Concorde.Work.Work_Priority :=
                               Manager.Work_Queue.First_Key;
                  Work     : constant Concorde.Work.Work_Item :=
                               Manager.Work_Queue.First_Element;
               begin
                  Manager.Work_Queue.Delete_First;

                  if Ministry.Has_Delegated_Power (Work.Power) then

                     declare
                        use Concorde.People.Individuals;
                        B : constant Concorde.Bureaucracy.Bureaucracy_Type :=
                              Ministry.Find_With_Power (Work.Power);
                        Director : constant Individual_Type :=
                                     Individual_Type (B.Director);
                     begin
                        Manager.Individual.Log
                          ("work",
                           "delegated to "
                           & B.Identifier
                           & " headed by "
                           & (if Director = null then "nobody"
                             else Director.Full_Name));

                        if Director /= null then
                           Director.Manager.Add_Work_Item (Work);
                        else
                           Remaining_Work.Insert (Priority, Work);
                        end if;
                     end;
                  else
                     Remaining_Work.Insert (Priority, Work);
                  end if;
               end;
            end loop;

            Manager.Work_Queue := Remaining_Work;
         end;
      end if;

      if not Manager.Work_Queue.Is_Empty then
         Manager.Current_Work :=
           Manager.Work_Queue.First_Element;
         Manager.Work_Queue.Delete_First;
         Concorde.Objects.Queues.Next_Event
           (Object => Manager.Individual,
            Date   =>
              Concorde.Calendar.Clock
            + Manager.Current_Work.Power.Execution_Work
              (Manager.Current_Work.Target));
      else
         Concorde.Objects.Queues.Next_Event
           (Manager.Individual, Manager.Time, Delay_Days => 1);
      end if;
   end On_Activated;

end Concorde.Managers.Individuals;
