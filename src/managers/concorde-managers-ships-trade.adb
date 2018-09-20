with WL.Heaps;
with Concorde.Money;
with Concorde.Quantities;

with Concorde.Locations;
with Concorde.Galaxy;

with Concorde.Objects.Queues;

package body Concorde.Managers.Ships.Trade is

   Detailed_Logging       : constant Boolean := True;

   procedure Create_Wanted_Commodities
     (Manager : in out Root_Ship_Trade_Manager'Class);

   --------------------
   -- Create_Manager --
   --------------------

   function Create_Manager
     (Ship    : Concorde.Ships.Ship_Type;
      Start   : Concorde.People.Communities.Community_Type)
      return Ship_Trade_Manager
   is
   begin
      return Manager : constant Ship_Trade_Manager :=
        new Root_Ship_Trade_Manager
      do
         Manager.Create (Ship);
         Manager.Community := Start;
         Manager.Next_Destination := Start;
      end return;
   end Create_Manager;

   -------------------------------
   -- Create_Wanted_Commodities --
   -------------------------------

   procedure Create_Wanted_Commodities
     (Manager : in out Root_Ship_Trade_Manager'Class)
   is

      package Community_Queues is
        new WL.Heaps (Real, Concorde.People.Communities.Community_Type,
                      "=" => Concorde.People.Communities."=");

      Queue : Community_Queues.Heap;

      Current_System : constant Concorde.Systems.Star_System_Type :=
                         Manager.Community.World.System;

      procedure Add_Wanted
        (Commodity : Concorde.Commodities.Commodity_Type);

      procedure Check_Community
        (Community : Concorde.People.Communities.Community_Type);

      procedure Choose_Community;

      ----------------
      -- Add_Wanted --
      ----------------

      procedure Add_Wanted
        (Commodity : Concorde.Commodities.Commodity_Type)
      is
         use Concorde.Money;
      begin
         if Manager.Next_Destination.Current_Price (Commodity)
           > Adjust_Price (Manager.Community.Current_Price (Commodity), 1.2)
         then
            Manager.Ship.Update.Add_Wanted
              (Commodity, Manager.Ship.Available_Capacity,
               Manager.Next_Destination.Current_Price (Commodity));
         end if;
      end Add_Wanted;

      ---------------------
      -- Check_Community --
      ---------------------

      procedure Check_Community
        (Community : Concorde.People.Communities.Community_Type)
      is
         use type Concorde.People.Communities.Community_Type;
      begin
         if Community /= Manager.Community then
            declare
               Score : constant Non_Negative_Real :=
                         10.0 / Concorde.Galaxy.Shortest_Path_Distance
                           (Current_System,
                            Community.World.System);
            begin
               Queue.Insert (Score, Community);
            end;
         end if;
      end Check_Community;

      ----------------------
      -- Choose_Community --
      ----------------------

      procedure Choose_Community is
      begin
         Manager.Next_Destination := Queue.First_Element;
         Manager.Ship.Log
           ("setting destination: "
            & Manager.Next_Destination.World.Name);
      end Choose_Community;

   begin
      Concorde.People.Communities.Scan
        (Check_Community'Access);

      Choose_Community;

      Concorde.Commodities.Scan
        (Add_Wanted'Access);
   end Create_Wanted_Commodities;

   -------------
   -- On_Idle --
   -------------

   overriding procedure On_Idle
     (Manager : in out Root_Ship_Trade_Manager)
   is
      use Concorde.Quantities;

   begin

      if Detailed_Logging then
         Manager.Ship.Log_Trade
           ("activated at "
            & Concorde.Calendar.Image (Manager.Time, True)
            & "; state = " & Manager.State'Img);
      end if;

      case Manager.State is
         when Bidding =>
            Manager.Create_Wanted_Commodities;
            if Manager.Ship.Has_Wanted then
               Manager.State := Buying;
            end if;
         when Buying =>
            declare
               Available : constant Quantity_Type :=
                             Manager.Ship.Available_Capacity;
               Wanted    : constant Boolean :=
                             Manager.Ship.Has_Wanted;
            begin
               if Available = Zero or else not Wanted then
                  Manager.Community.Update.Remove_Ship (Manager.Ship);
                  Manager.Ship.Update.Clear_Wanted;
                  Manager.Set_Destination (Manager.Next_Destination);
                  Manager.State := Moving;
               end if;
            end;
         when Moving =>
            Manager.State := Asking;
            Manager.Next_Destination.Update.Add_Ship (Manager.Ship);
            Manager.Next_Destination := null;

         when Asking =>
            Manager.State := Selling;

         when Selling =>
            if not Manager.Ship.Has_Offers then
               Manager.State := Bidding;
            end if;
      end case;

      if Manager.State /= Moving then
         declare
            use type Concorde.Calendar.Time;
         begin
            Concorde.Objects.Queues.Next_Event
              (Manager.Ship, Manager.Time + 86_400.0);
         end;
      end if;
   end On_Idle;

end Concorde.Managers.Ships.Trade;
