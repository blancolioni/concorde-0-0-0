with Concorde.Commodities;
with Concorde.Locations;
with Concorde.Money;
with Concorde.Quantities;
with Concorde.Trades;

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
      Remaining : Quantity_Type := Manager.Ship.Available_Quantity;
   begin
      for Commodity of Concorde.Commodities.Trade_Commodities loop
         declare
            Demand : constant Quantity_Type :=
                       Manager.Ship.Market.Get_Daily_Quantity
                         (Commodity, Concorde.Trades.Local_Demand, 7);
            Supply : constant Quantity_Type :=
                       Manager.Ship.Market.Get_Daily_Quantity
                         (Commodity, Concorde.Trades.Local_Supply, 7);
            Price  : constant Price_Type :=
                       Manager.Ship.Market.Current_Price (Commodity);
         begin
            if Supply > Demand then
               declare
                  Bid_Quantity : constant Quantity_Type :=
                                   Min (Supply - Demand, Remaining);
               begin
                  Manager.Ship.Log_Trade
                    (Commodity.Name
                     & ": 7 day demand: " & Image (Demand)
                     & "; supply: " & Image (Supply)
                     & "; current price "
                     & Image (Price));
                  Manager.Ship.Create_Bid
                    (Commodity, Bid_Quantity);
                  Remaining := Remaining - Bid_Quantity;
                  exit when Remaining = Zero;
               end;
            end if;
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
         & "; trading from " & From_World.Name & " to " & To_World.Name);

      if Manager.Ship.Available_Quantity = Zero then
         Next (Manager.Current);
         if not Has_Element (Manager.Current) then
            Manager.Current := Manager.Route.First;
         end if;
         Manager.Set_Destination (To_World);
      else
         if not Manager.Ship.Has_Bids then
            Manager.Create_Bids;
         end if;
         if not Manager.Ship.Has_Asks
           and then Manager.Ship.Total_Quantity > Zero
         then
            Manager.Create_Asks;
         end if;

         Concorde.Objects.Queues.Next_Event
           (Manager.Ship, Concorde.Dates.Add_Days (Manager.Time, 1));
      end if;
   end On_Idle;

end Concorde.Managers.Ships.Trade;
