with Concorde.Objects.Queues;
with Concorde.Signals.Standard;
with Concorde.Facilities;

package body Concorde.Managers.Ministries is

   ------------
   -- Create --
   ------------

   procedure Create
     (Manager : not null access Root_Ministry_Manager'Class;
      Ministry     : Concorde.Ministries.Ministry_Type)
   is
   begin
      Manager.Object := Ministry;
      Manager.Ministry := Ministry;
      Ministry.Update.Add_Handler
        (Concorde.Signals.Standard.Object_Activated,
         new Object_Activated_Handler'(Manager => Manager_Type (Manager)));
   end Create;

   --------------------
   -- Create_Manager --
   --------------------

   function Create_Manager
     (Ministry : Concorde.Ministries.Ministry_Type)
      return Ministry_Manager
   is
   begin
      return Manager : constant Ministry_Manager :=
        new Root_Ministry_Manager
      do
         Manager.Create (Ministry);
      end return;
   end Create_Manager;

   ------------
   -- Handle --
   ------------

   overriding procedure Handle
     (Handler : in out Root_Ministry_Event_Handler;
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
     (Manager : in out Root_Ministry_Manager)
   is
      use type Concorde.Calendar.Time;
      use type Concorde.Facilities.Facility_Type;
   begin
      Manager.Ministry.Log_Government
        ("activated; daily work is "
         & Concorde.Calendar.Image
           (Manager.Ministry.Daily_Work, True));
      Concorde.Objects.Queues.Next_Event
        (Manager.Ministry, Manager.Time + 86_400.0);
   end On_Activated;

end Concorde.Managers.Ministries;
