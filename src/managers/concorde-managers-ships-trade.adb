with WL.Heaps;

with Concorde.Locations;
with WL.Money;
with WL.Quantities;
with Concorde.Real_Images;

with Concorde.Markets;
with Concorde.Objects.Queues;

package body Concorde.Managers.Ships.Trade is

   package Commodity_Queues is
     new WL.Heaps (Real, Concorde.Commodities.Commodity_Type,
                   "=" => Concorde.Commodities."=");

   ------------------
   -- Add_Waypoint --
   ------------------

   procedure Add_Waypoint
     (Manager : in out Root_Ship_Trade_Manager'Class;
      World   : Concorde.Worlds.World_Type)
   is
   begin
      Manager.Route.Append (World);
   end Add_Waypoint;

   -----------------
   -- Create_Asks --
   -----------------

   procedure Create_Asks
     (Manager : in out Root_Ship_Trade_Manager'Class)
   is
      use WL.Quantities;
   begin
      for Commodity of Concorde.Commodities.Trade_Commodities loop
--           Manager.Ship.Log_Trade
--             (Commodity.Identifier & ": "
--              & (if Manager.Ship.Has_Bid (Commodity) then "(have bid) "
--                else "")
--              & "quantity " & Image (Manager.Ship.Get_Quantity (Commodity)));

         if not Manager.Ship.Has_Bid (Commodity)
           and then Manager.Ship.Get_Quantity (Commodity) > Zero
         then
            Manager.Ship.Create_Ask
              (Commodity, Manager.Ship.Get_Quantity (Commodity));
         end if;
      end loop;
   end Create_Asks;

   -----------------
   -- Create_Bids --
   -----------------

   procedure Create_Bids
     (Manager : in out Root_Ship_Trade_Manager'Class)
   is
      use Concorde.Commodities;
      use WL.Money, WL.Quantities;
      Space : constant Quantity_Type := Manager.Ship.Available_Quantity;
      Remaining    : Quantity_Type := Space;
      Local_Market : constant Concorde.Markets.Market_Type :=
                       Concorde.Markets.Market_Type (Manager.Ship.Market);
      Next_Market  : constant Concorde.Markets.Market_Type :=
                       Manager.Next_Destination.Market;
      Fixed_Costs  : constant Money_Type := To_Money (50.0);

      Queue        : Commodity_Queues.Heap;

      procedure Check_Markets
        (Commodity          : Concorde.Commodities.Commodity_Type;
         Current_Market     : Concorde.Markets.Market_Type;
         Destination_Market : Concorde.Markets.Market_Type);

      -------------------
      -- Check_Markets --
      -------------------

      procedure Check_Markets
        (Commodity          : Concorde.Commodities.Commodity_Type;
         Current_Market     : Concorde.Markets.Market_Type;
         Destination_Market : Concorde.Markets.Market_Type)
      is
         use Concorde.Calendar;
         Local_Demand    : constant Quantity_Type :=
                             Current_Market.Get_Daily_Quantity
                               (Commodity, Concorde.Trades.Local_Demand, 7)
                               with Unreferenced;
         Local_Supply    : constant Quantity_Type :=
                             Current_Market.Get_Daily_Quantity
                               (Commodity, Concorde.Trades.Local_Supply, 7);
         Next_Demand     : constant Quantity_Type :=
                             Destination_Market.Get_Daily_Quantity
                               (Commodity, Concorde.Trades.Local_Demand, 7);
         Next_Supply     : constant Quantity_Type :=
                             Destination_Market.Get_Daily_Quantity
                               (Commodity, Concorde.Trades.Local_Supply, 7)
                               with Unreferenced;
         Local_Price     : constant Price_Type :=
                             Adjust_Price
                               (Current_Market.Current_Price (Commodity),
                                1.1);
         Next_Price      : constant Price_Type :=
                             Adjust_Price
                               (Destination_Market.Current_Price (Commodity),
                                0.9);
         Traded_Quantity : constant Quantity_Type :=
                             (if Next_Demand > Zero
                              then Min (Space, Min (Local_Supply, Next_Demand))
                              else Zero);
      begin

         if Traded_Quantity > Zero and then Next_Price > Local_Price then
            declare
               Investment      : constant Money_Type :=
                                   Total (Local_Price, Traded_Quantity);
               Expected_Return : constant Money_Type :=
                                   Total (Next_Price, Traded_Quantity);
               Score           : constant Non_Negative_Real :=
                                   (if Expected_Return - Investment
                                    > Fixed_Costs
                                    then Real (To_Float (Expected_Return))
                                    else Real (To_Float (Expected_Return))
                                    - Real (To_Float (Investment)));
            begin
               Manager.Ship.Log
                 (Commodity.Name
                  & ": local supply: " & Image (Local_Supply)
                  & "; remote demand: " & Image (Next_Demand)
                  & "; local price: " & Image (Local_Price)
                  & "; remote price: " & Image (Next_Price)
                  & "; investment: " & Image (Investment)
                  & "; return: " & Image (Expected_Return)
                  & "; score: "
                  & Concorde.Real_Images.Approximate_Image (Score));
               Queue.Insert (Score, Commodity);
            end;
         end if;
      end Check_Markets;

   begin

      for Commodity of Concorde.Commodities.Trade_Commodities loop
         Check_Markets (Commodity, Local_Market, Next_Market);
      end loop;

      while not Queue.Is_Empty loop
         declare
            Commodity : constant Commodity_Type := Queue.Maximum_Element;
            Local_Supply : constant Quantity_Type :=
                             Local_Market.Get_Daily_Quantity
                               (Commodity, Concorde.Trades.Local_Supply, 7);
            Next_Demand     : constant Quantity_Type :=
                                Next_Market.Get_Daily_Quantity
                                  (Commodity, Concorde.Trades.Local_Demand, 7);
            Bid_Quantity    : constant Quantity_Type :=
                             Min (Next_Demand, Min (Local_Supply, Remaining));
         begin
            Queue.Delete_Maximum;
            Manager.Ship.Create_Bid
              (Commodity, Bid_Quantity);
            Remaining := Remaining - Bid_Quantity;
         end;
      end loop;

   end Create_Bids;

   --------------------
   -- Create_Manager --
   --------------------

   function Create_Manager
     (Ship    : Concorde.Ships.Ship_Type;
      Start   : Concorde.Worlds.World_Type)
      return Ship_Trade_Manager
   is
   begin
      return Manager : constant Ship_Trade_Manager :=
        new Root_Ship_Trade_Manager
      do
         Manager.Create (Ship);
         Manager.Route.Append (Start);
         Manager.Current := Manager.Route.First;
      end return;
   end Create_Manager;

   --------------------
   -- Create_Manager --
   --------------------

   function Create_Manager
     (Ship     : Concorde.Ships.Ship_Type;
      From, To : Concorde.Worlds.World_Type)
      return Ship_Trade_Manager
   is
   begin
      return Manager : constant Ship_Trade_Manager :=
        Create_Manager (Ship, From)
      do
         Manager.Add_Waypoint (To);
      end return;
   end Create_Manager;

   ---------------------
   -- Delete_Waypoint --
   ---------------------

   procedure Delete_Waypoint
     (Manager : in out Root_Ship_Trade_Manager'Class;
      World   : Concorde.Worlds.World_Type)
   is
      Position : World_Lists.Cursor := Manager.Route.Find (World);
   begin
      pragma Assert (World_Lists.Has_Element (Position));
      Manager.Route.Delete (Position);
   end Delete_Waypoint;

   --------------
   -- Has_Asks --
   --------------

   function Has_Asks
     (Manager : Root_Ship_Trade_Manager'Class)
      return Boolean
   is
   begin
      for Commodity of Concorde.Commodities.Trade_Commodities loop
         if Manager.Ship.Has_Ask (Commodity) then
            return True;
         end if;
      end loop;
      return False;
   end Has_Asks;

   --------------
   -- Has_Bids --
   --------------

   function Has_Bids (Manager : Root_Ship_Trade_Manager'Class) return Boolean
   is
   begin
      for Commodity of Concorde.Commodities.Trade_Commodities loop
         if Manager.Ship.Has_Bid (Commodity) then
            return True;
         end if;
      end loop;
      return False;
   end Has_Bids;

   ----------------------
   -- Next_Destination --
   ----------------------

   function Next_Destination
     (Manager : Root_Ship_Trade_Manager'Class)
      return Concorde.Worlds.World_Type
   is
      use World_Lists;
      Next_Position : Cursor := Next (Manager.Current);
   begin
      if not Has_Element (Next_Position) then
         Next_Position := Manager.Route.First;
      end if;
      return Element (Next_Position);
   end Next_Destination;

   -------------
   -- On_Idle --
   -------------

   overriding procedure On_Idle
     (Manager : in out Root_Ship_Trade_Manager)
   is
      use WL.Quantities;
      use Concorde.Worlds, World_Lists;
      From_World : constant World_Type := Element (Manager.Current);
      Next_Position : Cursor := Next (Manager.Current);
      To_World   : World_Type;
   begin
      if not Has_Element (Next_Position) then
         Next_Position := Manager.Route.First;
      end if;

      To_World := Element (Next_Position);

      Manager.Ship.Log_Trade
        ("activated at "
         & Concorde.Calendar.Image (Manager.Time, True)
         & "; state = " & Manager.State'Img
         & "; trading from " & From_World.Name & " to " & To_World.Name);

      case Manager.State is
         when Bidding =>
            Manager.Create_Bids;
            if Manager.Has_Bids then
               Manager.State := Buying;
            elsif Manager.Ship.Total_Quantity > Zero then
               Manager.Ship.Update.Clear_Filled_Bids;
               Manager.Current := Next_Position;
               Manager.Set_Destination (To_World);
               Manager.State := Moving;
            end if;
         when Buying =>
            if not Manager.Has_Bids then
               Manager.Ship.Update.Clear_Filled_Bids;
               Manager.Current := Next_Position;
               Manager.Set_Destination (To_World);
               Manager.State := Moving;
            else
               Manager.Ship.Update.Check_Offers;
            end if;
         when Moving =>
            Manager.State := Asking;
         when Asking =>
            Manager.Create_Asks;
            Manager.State := Selling;
         when Selling =>
            if Manager.Ship.Total_Quantity = Zero then
               Manager.Ship.Update.Clear_Filled_Asks;
               Manager.State := Bidding;
            else
               Manager.Ship.Update.Check_Offers;
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
