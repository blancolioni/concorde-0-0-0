with Concorde.Money;

with Concorde.Objects.Queues;
with Concorde.Signals.Standard;

with Concorde.Commodities;
with Concorde.Facilities;

package body Concorde.Managers.Corporations is

   ------------
   -- Create --
   ------------

   procedure Create
     (Manager : not null access Root_Corporation_Manager'Class;
      Corporation     : Concorde.Corporations.Corporation_Type)
   is
   begin
      Manager.Object := Corporation;
      Manager.Corporation := Corporation;
      Corporation.Update.Add_Handler
        (Concorde.Signals.Standard.Object_Activated,
         new Object_Activated_Handler'(Manager => Manager_Type (Manager)));
   end Create;

   --------------------
   -- Create_Manager --
   --------------------

   function Create_Manager
     (Corporation : Concorde.Corporations.Corporation_Type)
      return Corporation_Manager
   is
   begin
      return Manager : constant Corporation_Manager :=
        new Root_Corporation_Manager
      do
         Manager.Create (Corporation);
      end return;
   end Create_Manager;

   ------------
   -- Handle --
   ------------

   overriding procedure Handle
     (Handler : in out Root_Corporation_Event_Handler;
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
     (Manager : in out Root_Corporation_Manager)
   is
      use type Concorde.Calendar.Time;
      use Concorde.Money;
   begin
      Manager.Corporation.Log_Trade
        ("activated at "
         & Concorde.Calendar.Image (Manager.Time, True));

      if Manager.Corporation.Last_Earnings
        > Manager.Corporation.Last_Expenses
      then
         declare
            Profit : constant Money_Type :=
                       Manager.Corporation.Last_Earnings
                         - Manager.Corporation.Last_Expenses;
            Dividend : constant Money_Type :=
                         Adjust (Profit, 0.1);
         begin
            Manager.Corporation.Log_Trade
              ("profit: " & Show (Profit) & " dividend "
               & Show (Dividend));
            Manager.Corporation.Owner.Log_Trade
              ("earn " & Show (Dividend) & " dividend");
            Manager.Corporation.Update.Remove_Cash (Dividend);
            Manager.Corporation.Owner.Variable_Reference.Add_Cash
              (Dividend);
         end;
      end if;

      Manager.Corporation.Update.Perform_Work;

      Concorde.Objects.Queues.Next_Event
        (Manager.Corporation, Manager.Time + 86_400.0);
   end On_Activated;

end Concorde.Managers.Corporations;
