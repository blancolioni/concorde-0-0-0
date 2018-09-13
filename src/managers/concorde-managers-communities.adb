with WL.Money;

with Concorde.Logging;
with Concorde.Objects.Queues;
with Concorde.Signals.Standard;

with Concorde.Commodities;
with Concorde.Facilities;
with Concorde.People.Pops;
with Concorde.Policies;
with Concorde.Trades;

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

      declare
         Tax_Income   : Non_Negative_Real := 0.0;
         Total_Income : Non_Negative_Real := 0.0;
         GDP          : Non_Negative_Real := 0.0;

         procedure Update_Tax_Income
           (Policy : Concorde.Policies.Policy_Type);

         procedure Update_Total_Income
           (Pop : Concorde.People.Pops.Pop_Type);

         -----------------------
         -- Update_Tax_Income --
         -----------------------

         procedure Update_Tax_Income
           (Policy : Concorde.Policies.Policy_Type)
         is
         begin
            if Policy.Tax_Income.Show /= "()" then
               declare
                  Tax_Rate : constant Unit_Real :=
                               Manager.Community.Node (Policy.Identifier)
                               .Current_Value;
                  This_Tax : constant Real :=
                               Policy.Tax_Income.Evaluate
                                 (Env            => Manager.Community.all,
                                  Argument_Name  => "current-actual",
                                  Argument_Value => Tax_Rate);
               begin
--                    Concorde.Logging.Log
--                      (Actor    => Manager.Community.Identifier,
--                       Location => "tax income",
--                       Category => Policy.Identifier,
--                       Message  => Policy.Tax_Income.Show);
--
--                    Concorde.Logging.Log
--                      (Actor    => Manager.Community.Identifier,
--                       Location => "tax income",
--                       Category => Policy.Identifier,
--                       Message  =>
--                         "rate" & Natural'Image (Natural (Tax_Rate * 100.0))
--                       & "% total "
--                       & WL.Money.Show
--                         (WL.Money.To_Money
--                              (Float (This_Tax) / 365.0)));
                  Tax_Income := Tax_Income + This_Tax / 365.0;
               end;
            end if;
         end Update_Tax_Income;

         -------------------------
         -- Update_Total_Income --
         -------------------------

         procedure Update_Total_Income
           (Pop : Concorde.People.Pops.Pop_Type)
         is
         begin
            Total_Income := Total_Income + Pop.Current_Income_Total;
         end Update_Total_Income;

      begin
         Concorde.Policies.Scan_Policies (Update_Tax_Income'Access);

         Concorde.Logging.Log
           (Actor    => Manager.Community.Identifier,
            Location => "tax income",
            Category => "total",
            Message  =>
              WL.Money.Show
                (WL.Money.To_Money
                     (Float (Tax_Income))));

         Manager.Community.Scan_Pops (Update_Total_Income'Access);

         GDP := GDP + Total_Income;

         Concorde.Logging.Log
           (Actor    => Manager.Community.Identifier,
            Location => "income",
            Category => "total",
            Message  =>
              WL.Money.Show
                (WL.Money.To_Money
                     (Float (Total_Income / 365.0))));
         Concorde.Logging.Log
           (Actor    => Manager.Community.Identifier,
            Location => "economy",
            Category => "gdp",
            Message  =>
              WL.Money.Show
                (WL.Money.To_Money
                     (Float (GDP))));
      end;

      Concorde.Objects.Queues.Next_Event
        (Manager.Community, Manager.Time, Delay_Days => 1);
   end On_Activated;

end Concorde.Managers.Communities;
