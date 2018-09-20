with Concorde.Money;

with Concorde.Objects.Queues;
with Concorde.Signals.Standard;
with Concorde.Facilities;

package body Concorde.Managers.Industries is

   ------------
   -- Create --
   ------------

   procedure Create
     (Manager : not null access Root_Industry_Manager'Class;
      Industry     : Concorde.Industries.Industry_Type)
   is
   begin
      Manager.Object := Industry;
      Manager.Industry := Industry;
      Industry.Update.Add_Handler
        (Concorde.Signals.Standard.Object_Activated,
         new Object_Activated_Handler'(Manager => Manager_Type (Manager)));
   end Create;

   --------------------
   -- Create_Manager --
   --------------------

   function Create_Manager
     (Industry : Concorde.Industries.Industry_Type)
      return Industry_Manager
   is
   begin
      return Manager : constant Industry_Manager :=
        new Root_Industry_Manager
      do
         Manager.Create (Industry);
      end return;
   end Create_Manager;

   ------------
   -- Handle --
   ------------

   overriding procedure Handle
     (Handler : in out Root_Industry_Event_Handler;
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
     (Manager : in out Root_Industry_Manager)
   is
      use type Concorde.Calendar.Time;
      use Concorde.Money;
   begin
      Manager.Industry.Log_Trade
        ("activated at "
         & Concorde.Calendar.Image (Manager.Time, True));

      if Manager.Industry.Last_Earnings
        > Manager.Industry.Last_Expenses
      then
         declare
            Profit : constant Money_Type :=
                       Manager.Industry.Last_Earnings
                         - Manager.Industry.Last_Expenses;
            Dividend : constant Money_Type :=
                         Adjust (Profit, 0.1);
         begin
            Manager.Industry.Log_Trade
              ("profit: " & Show (Profit) & " dividend "
               & Show (Dividend));
            Manager.Industry.Owner.Log_Trade
              ("earn " & Show (Dividend) & " dividend");
            Manager.Industry.Update.Remove_Cash (Dividend);
            Manager.Industry.Owner.Variable_Reference.Add_Cash
              (Dividend);
         end;
      end if;

      Manager.Industry.Update.Execute_Production;

      Concorde.Objects.Queues.Next_Event
        (Manager.Industry, Manager.Time + 86_400.0);
   end On_Activated;

end Concorde.Managers.Industries;
