with Concorde.Objects.Queues;
with Concorde.Signals.Standard;

with Concorde.Commodities;
with Concorde.Facilities;
with Concorde.People.Pops;
with Concorde.Trades;

with Concorde.Worlds;

with Concorde.Markets.Reports;

package body Concorde.Managers.Communities is

   ------------
   -- Create --
   ------------

   procedure Create
     (Manager   : not null access Root_Community_Manager'Class;
      Community : Concorde.People.Communities.Community_Type)
   is
   begin
      Manager.Object := Community;
      Manager.Community := Community;
      Community.Update.Add_Handler
        (Concorde.Signals.Standard.Object_Activated,
         new Object_Activated_Handler'(Manager => Manager_Type (Manager)));
   end Create;

   --------------------
   -- Create_Manager --
   --------------------

   function Create_Manager
     (Community : Concorde.People.Communities.Community_Type)
      return Community_Manager
   is
   begin
      return Manager : constant Community_Manager :=
        new Root_Community_Manager
      do
         Manager.Create (Community);
      end return;
   end Create_Manager;

   ------------
   -- Handle --
   ------------

   overriding procedure Handle
     (Handler : in out Root_Community_Event_Handler;
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
     (Manager : in out Root_Community_Manager)
   is
   begin
      Manager.Community.Log
        ("Community on " & Manager.Community.World.Name
         & " activated at "
         & Concorde.Calendar.Image
           (Manager.Time, True));

--        Manager.Community.Update.Run_Network_State;
      Manager.Community.Update.Update_Market;

      Concorde.Markets.Reports.Log_Market_State
        (Manager.Community.all);

      Concorde.Objects.Queues.Next_Event
        (Manager.Community, Manager.Time, Delay_Days => 1);
   end On_Activated;

end Concorde.Managers.Communities;
