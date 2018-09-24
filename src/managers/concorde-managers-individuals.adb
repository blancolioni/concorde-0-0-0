with Concorde.Calendar;
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

   procedure Schedule_Work
     (Manager    : in out Root_Individual_Manager'Class);

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
      use type Concorde.Work.Work_Item;
      use type Concorde.Calendar.Time;
      Delayed_Work : Work_Item_Queue.Heap;
   begin

      if not Manager.Started then
         Manager.Started := True;
         Manager.Day_Start := Concorde.Calendar.Clock;
      end if;

      if Manager.Current_Work /= null then
         Manager.Individual.Log
           ("executing: " & Manager.Current_Work.Show);
         Concorde.People.Individuals.Work.Perform_Work
           (Manager.Individual, Manager.Current_Work);
         Manager.Current_Work := null;
      end if;

      if not Manager.Daily_Queue.Is_Empty then
         Manager.Individual.Log ("have more daily work");
         Manager.Schedule_Work;
         return;
      end if;

      Manager.Individual.Log ("no daily work");

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
                        if Director /= null then
                           Manager.Individual.Log
                             ("work",
                              Work.Show
                              & " delegated to "
                              & B.Identifier
                              & " headed by "
                              & Director.Full_Name);
                           B.Variable_Reference.Add_Work_Item (Work);
                        else
                           Manager.Individual.Log
                             ("work",
                              Work.Show
                              & " for "
                              & B.Identifier
                              & " put on hold until minister appointed");
                           Delayed_Work.Insert (Priority, Work);
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
         declare
            Remaining_Time : Duration :=
                               Concorde.Calendar.Hours (16);
         begin
            Manager.Individual.Log ("creating daily work queue");

            while not Manager.Work_Queue.Is_Empty loop
               declare
                  Work : constant Concorde.Work.Work_Item :=
                           Manager.Work_Queue.First_Element;
                  Priority : constant Concorde.Work.Work_Priority :=
                               Manager.Work_Queue.First_Key;
                  Time     : constant Duration :=
                               (if Work.Power.Daily_Execution
                                then Work.Power.Daily_Work
                                  (Manager.Individual.Current_Community)
                                else Work.Power.Execution_Work
                                  (Work.Target));
               begin
                  Manager.Work_Queue.Delete_First;
                  Manager.Daily_Queue.Insert (Priority, Work);
                  Manager.Individual.Log ("queuing: " & Work.Show);
                  exit when Time > Remaining_Time;
                  Remaining_Time := Remaining_Time - Time;
               end;
            end loop;

            while not Delayed_Work.Is_Empty loop
               Manager.Work_Queue.Insert
                 (Delayed_Work.First_Key, Delayed_Work.First_Element);
               Delayed_Work.Delete_First;
            end loop;

            Manager.Day_Start :=
              Manager.Day_Start + Concorde.Calendar.Days (1);
            Concorde.Objects.Queues.Next_Event
              (Manager.Individual, Manager.Day_Start);

            Manager.Individual.Log
              ("starting again at "
               & Concorde.Calendar.Image
                 (Manager.Day_Start,
                  Include_Time_Fraction => True));
            return;
         end;

      end if;

      pragma Assert (Manager.Work_Queue.Is_Empty);

      while not Delayed_Work.Is_Empty loop
         Manager.Work_Queue.Insert
           (Delayed_Work.First_Key, Delayed_Work.First_Element);
         Delayed_Work.Delete_First;
      end loop;

      pragma Assert (Manager.Daily_Queue.Is_Empty);

      Manager.Day_Start :=
        Manager.Day_Start + Concorde.Calendar.Days (1);
      Concorde.Objects.Queues.Next_Event
        (Manager.Individual, Manager.Day_Start);

   end On_Activated;

   -------------------
   -- Schedule_Work --
   -------------------

   procedure Schedule_Work
     (Manager    : in out Root_Individual_Manager'Class)
   is
      use type Concorde.Calendar.Time;

      Work     : constant Concorde.Work.Work_Item :=
                   Manager.Daily_Queue.First_Element;
      Priority : constant Concorde.Work.Work_Priority :=
                   Manager.Daily_Queue.First_Key;
      Time     : constant Duration :=
                   (if Work.Power.Daily_Execution
                    then Work.Power.Daily_Work
                      (Manager.Individual.Current_Community)
                    else Work.Power.Execution_Work
                      (Work.Target));
   begin
      Manager.Individual.Log
        ("current work: " & Work.Show
         & " will take "
         & Natural'Image (Natural (Time / 3600.0))
         & " hours");
      Manager.Current_Work := Work;
      Manager.Daily_Queue.Delete_First;

      if Work.Power.Daily_Execution then
         Manager.Work_Queue.Insert (Priority, Work);
      end if;

      Concorde.Objects.Queues.Next_Event
        (Object => Manager.Individual,
         Date   => Concorde.Calendar.Clock + Time);
   end Schedule_Work;

end Concorde.Managers.Individuals;
