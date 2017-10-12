with Concorde.Objects.Queues;
with Concorde.Signals.Standard;

package body Concorde.Managers.Installations is

   ------------
   -- Create --
   ------------

   procedure Create
     (Manager : not null access Root_Installation_Manager'Class;
      Installation     : Concorde.Installations.Installation_Type)
   is
   begin
      Manager.Object := Installation;
      Manager.Installation := Installation;
      Installation.Update.Add_Handler
        (Concorde.Signals.Standard.Object_Activated,
         new Object_Activated_Handler'(Manager => Manager_Type (Manager)));
   end Create;

   --------------------
   -- Create_Manager --
   --------------------

   function Create_Manager
     (Installation : Concorde.Installations.Installation_Type)
      return Installation_Manager
   is
   begin
      return Manager : constant Installation_Manager :=
        new Root_Installation_Manager
      do
         Manager.Create (Installation);
      end return;
   end Create_Manager;

   ------------
   -- Handle --
   ------------

   overriding procedure Handle
     (Handler : in out Root_Installation_Event_Handler;
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
     (Manager : in out Root_Installation_Manager)
   is
      use type Concorde.Calendar.Time;
   begin
      Manager.Installation.Log_Trade
        ("activated at "
         & Concorde.Calendar.Image (Manager.Time, True));
      Manager.Installation.Update.Check_Offers;
      Manager.Installation.Add_Trade_Offers;
      Manager.Installation.Update.Execute_Production;
      Manager.Installation.Update.Pay_Workers;
      Concorde.Objects.Queues.Next_Event
        (Manager.Installation, Manager.Time + 86_400.0);
   end On_Activated;

end Concorde.Managers.Installations;
