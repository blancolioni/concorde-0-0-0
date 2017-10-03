with Concorde.Locations;
with Concorde.Money;

with Concorde.Objects.Queues;

package body Concorde.Managers.Ships.Trade is

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

   -------------------
   -- Check_Markets --
   -------------------

   procedure Check_Markets
     (Manager            : in out Root_Ship_Trade_Manager'Class;
      Commodity          : Concorde.Commodities.Commodity_Type;
      Current_Market     : Concorde.Markets.Market_Type;
      Destination_Market : Concorde.Markets.Market_Type;
      Space              : in out Concorde.Quantities.Quantity_Type)
   is
      use Concorde.Dates;
      use Concorde.Money;
      use Concorde.Quantities;
      Local_Demand : constant Quantity_Type :=
                       Current_Market.Get_Daily_Quantity
                         (Commodity, Concorde.Trades.Local_Demand, 7);
      Local_Supply : constant Quantity_Type :=
                       Current_Market.Get_Daily_Quantity
                         (Commodity, Concorde.Trades.Local_Supply, 7);
      Next_Demand  : constant Quantity_Type :=
                       Destination_Market.Get_Quantity
                         (Commodity, Concorde.Trades.Local_Demand,
                          Add_Days (Manager.Time, -7), Manager.Time);
      Next_Supply  : constant Quantity_Type :=
                       Destination_Market.Get_Quantity
                         (Commodity, Concorde.Trades.Local_Supply,
                          Add_Days (Manager.Time, -7), Manager.Time);
      Local_Price  : constant Price_Type :=
                       Current_Market.Current_Price (Commodity);
      Next_Price   : constant Price_Type :=
                       Destination_Market.Current_Price (Commodity);
   begin
      if Local_Supply > Local_Demand then
         Manager.Ship.Log_Trade
           (Commodity.Name
            & ": 7 day " & Current_Market.Name
            & " demand: " & Image (Local_Demand)
            & "; supply: " & Image (Local_Supply)
            & "; price "
            & Image (Local_Price));
         Manager.Ship.Log_Trade
           (Commodity.Name
            & ": 7 day " & Destination_Market.Name
            & " demand: " & Image (Next_Demand)
            & "; supply: " & Image (Next_Supply)
            & "; price "
            & Image (Next_Price));
      end if;

      if Local_Supply > Local_Demand
        and then Next_Demand >= Next_Supply
        and then Next_Price > Local_Price
      then
         declare
            Bid_Quantity : constant Quantity_Type :=
                             Min
                               (Next_Demand,
                                Min
                                  (Local_Supply - Local_Demand,
                                   Space));
         begin
            Manager.Ship.Create_Bid
              (Commodity, Bid_Quantity);
            Space := Space - Bid_Quantity;
         end;
      end if;
   end Check_Markets;

   -----------------
   -- Create_Asks --
   -----------------

   procedure Create_Asks
     (Manager : in out Root_Ship_Trade_Manager'Class)
   is
      use Concorde.Quantities;
   begin
      for Commodity of Concorde.Commodities.Trade_Commodities loop
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
      use Concorde.Money, Concorde.Quantities;
      Remaining    : Quantity_Type := Manager.Ship.Available_Quantity;
      Local_Market : constant Concorde.Markets.Market_Type :=
                       Concorde.Markets.Market_Type (Manager.Ship.Market);
      Next_Market  : constant Concorde.Markets.Market_Type :=
                       Manager.Next_Destination.Market;
   begin
      for Commodity of Concorde.Commodities.Trade_Commodities loop
         Manager.Check_Markets
           (Commodity, Local_Market, Next_Market, Remaining);
         exit when Remaining = Zero;
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
      use Concorde.Quantities;
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
         & Concorde.Dates.To_Date_And_Time_String (Manager.Time)
         & "; state = " & Manager.State'Img
         & "; trading from " & From_World.Name & " to " & To_World.Name);

      case Manager.State is
         when Bidding =>
            Manager.Create_Bids;
            if Manager.Ship.Has_Bids then
               Manager.State := Buying;
            elsif Manager.Ship.Total_Quantity > Zero then
               Manager.Current := Next_Position;
               Manager.Set_Destination (To_World);
               Manager.State := Moving;
            end if;
         when Buying =>
            if not Manager.Ship.Has_Bids then
               Manager.Current := Next_Position;
               Manager.Set_Destination (To_World);
               Manager.State := Moving;
            end if;
         when Moving =>
            Manager.State := Asking;
         when Asking =>
            Manager.Create_Asks;
            Manager.State := Selling;
         when Selling =>
            if Manager.Ship.Total_Quantity = Zero then
               Manager.State := Bidding;
            end if;
      end case;

      if Manager.State /= Moving then
         Concorde.Objects.Queues.Next_Event
           (Manager.Ship, Concorde.Dates.Add_Days (Manager.Time, 1));
      end if;
   end On_Idle;

end Concorde.Managers.Ships.Trade;
