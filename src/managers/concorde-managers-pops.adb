with Concorde.Objects.Queues;
with Concorde.Signals.Standard;

package body Concorde.Managers.Pops is

   ------------
   -- Create --
   ------------

   procedure Create
     (Manager : not null access Root_Pop_Manager'Class;
      Pop     : Concorde.People.Pops.Pop_Type)
   is
   begin
      Manager.Object := Pop;
      Manager.Pop := Pop;
      Pop.Update.Add_Handler
        (Concorde.Signals.Standard.Object_Activated,
         new Object_Activated_Handler'(Manager => Manager_Type (Manager)));
   end Create;

   --------------------
   -- Create_Manager --
   --------------------

   function Create_Manager
     (Pop : Concorde.People.Pops.Pop_Type)
      return Pop_Manager
   is
   begin
      return Manager : constant Pop_Manager := new Root_Pop_Manager do
         Manager.Create (Pop);
      end return;
   end Create_Manager;

   ------------
   -- Handle --
   ------------

   overriding procedure Handle
     (Handler : in out Root_Pop_Event_Handler;
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
     (Manager : in out Root_Pop_Manager)
   is
   begin
      Manager.Pop.Log_Trade
        ("activated at "
         & Concorde.Calendar.Image
           (Manager.Time, True));
      Manager.Pop.Add_Trade_Offers;
      Manager.Pop.Update.Execute_Consumption;
      Concorde.Objects.Queues.Next_Event
        (Manager.Pop, Manager.Time, Delay_Days => 1);
   end On_Activated;

end Concorde.Managers.Pops;
