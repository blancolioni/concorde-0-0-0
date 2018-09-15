with WL.Money;

with Concorde.Objects.Queues;
with Concorde.Signals.Standard;

with Concorde.Commodities;
with Concorde.Facilities;
with Concorde.People.Pops;
with Concorde.Trades;

with Concorde.Network;

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

      Manager.Community.Update.Run_Network_State;

      Manager.Community.Log
        ("agriculature industry value: "
         & WL.Money.Show
           (WL.Money.To_Money
                (Float
                     (Concorde.Network.To_Real_Value
                        (Manager.Community.Node
                           ("agriculture-industry")
                         .Get_Field_Value ("value"))))));

      Manager.Community.Log
        ("tax income: "
         & WL.Money.Show
           (WL.Money.To_Money
                (Float
                     (Manager.Community.Node
                        ("tax-income").Current_Actual_Value) / 365.0))
         & "; military budget: "
         & WL.Money.Show
           (WL.Money.To_Money
                (Float
                     (Concorde.Network.To_Real_Value
                        (Manager.Community.Node
                           ("military-budget").Get_Field_Value ("budget")))))
         & " (daily "
         & WL.Money.Show
           (WL.Money.To_Money
                (Float
                     (Concorde.Network.To_Real_Value
                        (Manager.Community.Node
                           ("military-budget")
                         .Get_Field_Value ("daily-budget")))))
         & ")"
         & "; gdp: "
         & WL.Money.Show
           (WL.Money.To_Money
                (Float
                     (Manager.Community.Node
                        ("gdp").Current_Actual_Value)
                 / 365.0)));

      Concorde.Objects.Queues.Next_Event
        (Manager.Community, Manager.Time, Delay_Days => 1);
   end On_Activated;

end Concorde.Managers.Communities;
