with Ada.Containers.Vectors;

with Concorde.Money;
with Concorde.Quantities;

with Concorde.Objects.Queues;
with Concorde.Signals.Standard;

with Concorde.Commodities;
with Concorde.Facilities;
with Concorde.Trades;

with Concorde.People.Communities;
with Concorde.Worlds;

with Concorde.Logs;

package body Concorde.Managers.Pops is

   package Market_Commodity_Vectors is
     new Ada.Containers.Vectors
       (Positive, Concorde.Commodities.Commodity_Type,
        Concorde.Commodities."=");

   Market_Commodities : Market_Commodity_Vectors.Vector;

   type Pop_Logger is
     new Concorde.Logs.Log_Interface with
      record
         Pop : Concorde.People.Pops.Pop_Type;
      end record;

   overriding function Path (Log : Pop_Logger) return String
   is ("pops/" & Log.Pop.Current_Community.World.Identifier
       & "/" & Log.Pop.Identifier);

   overriding function Field_Count (Log : Pop_Logger) return Natural;

   overriding function Heading
     (Log   : Pop_Logger;
      Index : Positive)
      return String;

   overriding function Value
     (Log   : Pop_Logger;
      Index : Positive)
      return String;

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

   -----------------
   -- Field_Count --
   -----------------

   overriding function Field_Count
     (Log : Pop_Logger)
      return Natural
   is
      pragma Unreferenced (Log);
   begin
      return 2;
   end Field_Count;

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

   -------------
   -- Heading --
   -------------

   overriding function Heading
     (Log   : Pop_Logger;
      Index : Positive)
      return String
   is
      pragma Unreferenced (Log);
   begin
      if Index = 1 then
         return "Cash";
      elsif Index = 2 then
         return "Per Person";
      else
         return Market_Commodities.Element (Index - 2).Identifier;
      end if;
   end Heading;

   ------------------
   -- On_Activated --
   ------------------

   overriding procedure On_Activated
     (Manager : in out Root_Pop_Manager)
   is
      Happiness : Non_Negative_Real := 1.0;
   begin
      Manager.Pop.Log_Trade
        ("pop activated at "
         & Concorde.Calendar.Image
           (Manager.Time, True));

      if Market_Commodities.Is_Empty then
         for Item of Concorde.Commodities.All_Commodities loop
            Market_Commodities.Append (Item);
         end loop;
      end if;

      declare
         use Concorde.Money;
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
            & " (" & Show (Price  (Earnings, Manager.Pop.Size_Quantity)) & ")"
            & "; cash " & Show (Cash)
            & " (" & Show (Price  (Cash, Manager.Pop.Size_Quantity)) & ")"
            & "; expenses: "
            & Show (Manager.Pop.Last_Expenses)
            & " ("
            & Show
              (Price
                   (Manager.Pop.Last_Expenses,
                    Manager.Pop.Size_Quantity)) & ")"
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

         declare
            Income_Tax : constant Money_Type :=
                           Manager.Government.Income_Tax
                             (Income   => Earnings,
                              Quantity => Manager.Pop.Size_Quantity);
            Paid_Tax   : constant Money_Type :=
                           Min (Income_Tax, Manager.Pop.Cash);
         begin
            Manager.Pop.Log ("income tax: " & Show (Income_Tax)
                             & " ("
                             & Show
                               (Price
                                  (Income_Tax, Manager.Pop.Size_Quantity))
                             & ")"
                             & "; paid: " & Show (Paid_Tax)
                             & "; remaining: " & Show (Income_Tax - Paid_Tax));

            Manager.Government.Update.Tax_Receipt
              (Concorde.Government.Income_Tax, Paid_Tax);
            Manager.Pop.Update.Remove_Cash (Paid_Tax);
         end;

         for Commodity of Concorde.Commodities.All_Commodities loop
            declare
               use Concorde.Quantities;
               Required : constant Quantity_Type :=
                            Scale (Manager.Pop.Size_Quantity,
                                   Concorde.Commodities.Pop_Daily_Needs
                                     (Commodity));
               Available : constant Quantity_Type :=
                             Manager.Pop.Get_Quantity (Commodity);
            begin
               if Required > Zero then
                  declare
                     Factor : constant Real :=
                                To_Real (Available) / To_Real (Required);
                  begin
                     if Required <= Available then
                        Happiness := Happiness * 1.01;
                        Manager.Pop.Update.Remove_Quantity
                          (Commodity, Required);
                     else
                        Happiness := (Happiness + Factor) / 2.0;
                        Manager.Pop.Update.Set_Quantity
                          (Commodity, Zero, Zero);
                     end if;
                  end;
               end if;
            end;
         end loop;

         Manager.Happiness := Unit_Clamp (Happiness);
      end;

      declare
         Logger : constant Pop_Logger := (Pop => Manager.Pop);
      begin
         Concorde.Logs.Log (Logger);
      end;

      Manager.Pop.Update.Clear_Current_Account;
      Manager.Pop.Update.Clear_Flagged_Stock
        (Concorde.Commodities.Transient);

      Concorde.Objects.Queues.Next_Event
        (Manager.Pop, Manager.Time, Delay_Days => 1);
   end On_Activated;

   -----------
   -- Value --
   -----------

   overriding function Value
     (Log   : Pop_Logger;
      Index : Positive)
      return String
   is
   begin
      case Index is
         when 1 =>
            return Concorde.Money.Image (Log.Pop.Cash);
         when 2 =>
            return Concorde.Money.Image
              (Concorde.Money.Price (Log.Pop.Cash, Log.Pop.Size_Quantity));
         when others =>
            raise Program_Error;
      end case;
   end Value;

end Concorde.Managers.Pops;
