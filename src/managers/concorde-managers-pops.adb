with WL.Money;
with WL.Quantities;

with Concorde.Objects.Queues;
with Concorde.Signals.Standard;

with Concorde.Random;
with Concorde.Weighted_Random_Choices;

with Concorde.Commodities;
with Concorde.Facilities;
with Concorde.Trades;

package body Concorde.Managers.Pops is

   Minimum_Artisan_Production_Duration : constant Duration :=
                                           Concorde.Calendar.Day_Length
                                             * 5.0;

   -------------------------------
   -- Choose_Artisan_Production --
   -------------------------------

   procedure Choose_Artisan_Production
     (Manager : in out Root_Pop_Manager)
   is
      Market : constant access constant
        Concorde.Trades.Trade_Interface'Class :=
          Manager.Pop.Market;

      package Production_Choices is
        new Concorde.Weighted_Random_Choices
          (Concorde.Facilities.Facility_Type);

      Choices : Production_Choices.Weighted_Choice_Set;

      function Production_Score
        (Facility : Concorde.Facilities.Facility_Type)
         return Natural;

      ----------------------
      -- Production_Score --
      ----------------------

      function Production_Score
        (Facility : Concorde.Facilities.Facility_Type)
         return Natural
      is
         use WL.Money, WL.Quantities;
         Score : Real := 0.0;
      begin

         if Market.Current_Demand (Facility.Output)
           < Manager.Pop.Size_Quantity
         then
            Manager.Pop.Log ("artisan",
                             Facility.Name & ": no demand");
            return 0;
         end if;

         for Input_Index in 1 .. Facility.Input_Count loop
            declare
               OK : Boolean := False;
            begin
               for Choice_Index in
                 1 .. Facility.Input_Choice_Count (Input_Index)
               loop
                  declare
                     Input      : constant Commodities.Commodity_Type :=
                                    Facility.Input_Choice_Commodity
                                      (Input_Index, Choice_Index);
                     Required   : constant Quantity_Type :=
                                    Facility.Input_Choice_Quantity
                                      (Manager.Pop.Size_Quantity,
                                       Input_Index, Choice_Index);
                     Supply     : constant Quantity_Type :=
                                    Market.Current_Supply (Input);
                     Demand     : constant Quantity_Type :=
                                    Market.Current_Demand (Input);
                     Price      : constant Price_Type :=
                                    Market.Current_Price (Input);
                     This_Score : Real;
                  begin
                     Manager.Pop.Log ("artisan",
                                      Facility.Name & ": input "
                                      & Input.Name
                                      & ": supply/demand/price "
                                      & Show (Supply) & "/"
                                      & Show (Demand) & "/"
                                      & Show (Price));
                     if not Input.Available
                       or else Supply = Zero
                       or else Supply < Scale (Demand, 0.5)
                     then
                        null;
                     else
                        OK := True;
                        if Required + Demand < Supply then
                           This_Score := 100.0;
                        else
                           This_Score :=
                             Real (To_Float (Supply)
                                   / To_Float (Demand + Required))
                               * 100.0;
                        end if;

                        This_Score := This_Score
                          * Real (To_Float (Input.Base_Price)
                                  / To_Float (Price));
                        Score := Score + This_Score;
                     end if;
                  end;
               end loop;

               if not OK then
                  Manager.Pop.Log ("artisan",
                                   Facility.Name & ": no supply for input "
                                   & Facility.Input_Choice_Commodity
                                     (Input_Index, 1).Name);
                  return 0;
               end if;
            end;
         end loop;

         Score := Score
           * Real (To_Float (Market.Current_Price (Facility.Output))
                   / To_Float (Facility.Output.Base_Price));

         if Score <= 0.0 then
            return 0;
         else
            Manager.Pop.Log ("artisan",
                             Facility.Name & ": final score:"
                             & Natural'Image (Natural (100.0 * Score)));
            return Natural (Score * 100.0);
         end if;

      end Production_Score;

   begin

      Manager.Pop.Log ("artisan", "choosing production");

      for Facility of Concorde.Facilities.Artisan_Facilities loop
         Choices.Insert (Facility, Production_Score (Facility));
      end loop;

      if not Choices.Is_Empty then
         declare
            Choice : constant Concorde.Facilities.Facility_Type :=
                       Choices.Choose;
         begin
            Manager.Pop.Update.Set_Production (Choice);
            Manager.Pop.Log ("artisan",
                             "choose production: " & Choice.Name);
         end;
      else
         Manager.Pop.Log ("artisan", "nothing appropriate");
      end if;

   end Choose_Artisan_Production;

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
      Manager.Government :=
        Concorde.Government.Government_Type
          (Pop.Government);
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
      declare
         use WL.Money;
         Earnings : constant Money_Type := Manager.Pop.Last_Earnings;
         Cash     : constant Money_Type := Manager.Pop.Cash;
         Minimum  : constant Money_Type :=
                      Total (Manager.Government.Basic_Living_Wage,
                             Manager.Pop.Size_Quantity);
         Subsidy  : constant Money_Type :=
                      (if Earnings + Cash < Minimum
                       then Minimum - Earnings - Cash
                       else Zero);
      begin
         Manager.Pop.Log
           ("budget",
            "earnings: " & Show (Earnings)
            & "; cash " & Show (Cash)
            & "; expenses: "
            & Show (Manager.Pop.Last_Expenses)
            & "; minimum: " & Show (Minimum)
            & "; expected subsidy: " & Show (Subsidy));

         if Subsidy > Zero then
            Manager.Government.Update.Require_Cash (Subsidy);
            if Manager.Government.Cash >= Subsidy then
               Manager.Government.Update.Remove_Cash (Subsidy);
               Manager.Pop.Update.Add_Cash (Subsidy);
               Manager.Pop.Log_Trade
                 ("basic living wage: " & Show (Subsidy));
            else
               Manager.Government.Log
                 ("unable to pay basic living wage subsidy of "
                  & Show (Subsidy) & " to "
                  & Manager.Pop.Identifier);
            end if;
         end if;
         Manager.Pop.Update.Clear_Current_Account;
      end;

      if Manager.Pop.Group.Is_Artisan then
         if not Manager.Pop.Has_Production
           or else
             (Manager.Pop.Current_Production_Duration
              > Minimum_Artisan_Production_Duration
              and then Concorde.Random.Unit_Random < 0.2)
         then
            Manager.Choose_Artisan_Production;
         end if;
      end if;

      Manager.Pop.Update.Check_Offers;
      Manager.Pop.Add_Trade_Offers;
      Manager.Pop.Update.Execute_Consumption;
      Concorde.Objects.Queues.Next_Event
        (Manager.Pop, Manager.Time, Delay_Days => 1);
   end On_Activated;

end Concorde.Managers.Pops;
