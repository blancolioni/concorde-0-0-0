with WL.Heaps;
with WL.Money;
with WL.Quantities;

with Concorde.Locations;

with Concorde.Contracts;
with Concorde.Markets;
with Concorde.Objects.Queues;

package body Concorde.Managers.Ships.Trade is

   package Contract_Queues is
     new WL.Heaps (Real, Concorde.Contracts.Contract_Type,
                   "=" => Concorde.Contracts."=");

   -----------------
   -- Create_Asks --
   -----------------

   procedure Create_Asks
     (Manager : in out Root_Ship_Trade_Manager'Class)
   is
      use WL.Quantities;
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
      use Concorde.Commodities;
      use WL.Money, WL.Quantities;
      Local_Market   : constant Concorde.Markets.Market_Type :=
                         Concorde.Markets.Market_Type (Manager.Ship.Market);
      Contract_Queue : Contract_Queues.Heap;

      procedure Check_Contract
        (Contract : Concorde.Contracts.Contract_Type);

      --------------------
      -- Check_Contract --
      --------------------

      procedure Check_Contract
        (Contract : Concorde.Contracts.Contract_Type)
      is
         use Concorde.Contracts;
         use Concorde.Locations;
         use type Concorde.Objects.Object_Type;
         Local_Supply    : constant Quantity_Type :=
                             Local_Market.Current_Supply (Contract.Commodity);
         Local_Demand    : constant Quantity_Type :=
                             Local_Market.Current_Demand (Contract.Commodity);
      begin
--           Manager.Ship.Log_Trade
--             ("checking contract: " & Contract.Show);
--           Manager.Ship.Log_Trade
--             ("   local supply/demand: "
--              & Show (Local_Supply)
--              & "/"
--              & Show (Local_Demand));

         if Local_Supply > Zero then
            declare
               Local_Price     : constant Price_Type :=
                                   Manager.Ship.Create_Bid_Price
                                     (Contract.Commodity);
               Local_Cost      : constant Money_Type :=
                                   Total (Local_Price, Contract.Quantity);
            begin
               if Contract.Class = Buy_Goods
                 and then Contract.Quantity <= Manager.Ship.Hold_Quantity
                 and then Local_Supply > Zero
                 and then Primary (Contract.Location)
                 /= Primary (Manager.Ship.Current_Location)
               then
                  Manager.Ship.Log_Trade
                    ("checking contract: " & Contract.Show);
                  Manager.Ship.Log_Trade
                    ("   local supply/demand: "
                     & Show (Local_Supply)
                     & "/"
                     & Show (Local_Demand));
                  Manager.Ship.Log_Trade
                    ("   local price/cost/cash/limit: "
                     & Show (Local_Price)
                     & "/"
                     & Show (Local_Cost)
                     & "/"
                     & Show (Manager.Ship.Cash)
                     & "/"
                     & Show (Manager.Ship.Limit_Cash));

                  if Local_Price < Contract.Price
                    and then Contract.Quantity < Local_Supply
                    and then Local_Cost <= Manager.Ship.Limit_Cash
                  then
                     Manager.Ship.Log_Trade
                       ("accepted: "
                        & Contract.Show);
                     Contract_Queue.Insert
                       (Real (To_Float (Contract.Price - Local_Price)),
                        Contract);
                  end if;
               end if;
            end;
         end if;
      end Check_Contract;

   begin

      Concorde.Contracts.Scan_Available_Contracts
        (Check_Contract'Access);

      Manager.Next_Destination := null;

      while Manager.Ship.Contracted_Quantity < Manager.Ship.Hold_Quantity
        and then not Contract_Queue.Is_Empty
      loop
         declare
            use Concorde.Locations;
            use Concorde.Objects;
            use Concorde.Worlds;
            use Concorde.Contracts;
            Contract : constant Contract_Type :=
                         Contract_Queue.Maximum_Element;
         begin
            Contract_Queue.Delete_Maximum;
            if (Manager.Next_Destination = null
                or else Object_Type (Manager.Next_Destination)
                = Primary (Contract.Location))
              and then
                Contract.Quantity <=
                  Manager.Ship.Hold_Quantity
                    - Manager.Ship.Contracted_Quantity
            then
               Manager.Ship.Log_Trade
                 ("accepted: " & Contract.Show);
               Manager.Ship.Accept_Contract (Contract);
               Manager.Ship.Create_Bid
                 (Contract.Commodity, Contract.Quantity);
               Manager.Next_Destination :=
                 World_Type (Primary (Contract.Location));
            else
               Manager.Ship.Log_Trade
                 ("rejected: " & Contract.Show);
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
         Manager.Next_Destination := Start;
      end return;
   end Create_Manager;

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

   -------------
   -- On_Idle --
   -------------

   overriding procedure On_Idle
     (Manager : in out Root_Ship_Trade_Manager)
   is
      use WL.Quantities;
      use Concorde.Worlds, World_Lists;

      procedure Deliver_Goods
        (Contract : Concorde.Contracts.Contract_Type);

      -------------------
      -- Deliver_Goods --
      -------------------

      procedure Deliver_Goods
        (Contract : Concorde.Contracts.Contract_Type)
      is
      begin
         Manager.Ship.Log_Trade ("delivering: " & Contract.Show);
         Contract.Complete_Contract;
      end Deliver_Goods;

   begin

      Manager.Ship.Log_Trade
        ("activated at "
         & Concorde.Calendar.Image (Manager.Time, True)
         & "; state = " & Manager.State'Img);

      case Manager.State is
         when Bidding =>
            Manager.Create_Bids;
            if Manager.Has_Bids then
               Manager.State := Buying;
--              elsif Manager.Ship.Total_Quantity > Zero then
--                 Manager.Ship.Update.Clear_Filled_Bids;
--                 Manager.Set_Destination (Manager.Next_Destination);
--                 Manager.Next_Destination := null;
--                 Manager.State := Moving;
            end if;
         when Buying =>
            if not Manager.Has_Bids then
               Manager.Ship.Update.Clear_Filled_Bids;
               Manager.Set_Destination (Manager.Next_Destination);
               Manager.Next_Destination := null;
               Manager.State := Moving;
            else
               Manager.Ship.Update.Check_Offers;
            end if;
         when Moving =>
            Manager.State := Asking;
         when Asking =>
            Manager.Ship.Update.Scan_Accepted_Contracts
              (Deliver_Goods'Access);
            Manager.Ship.Update.Close_Completed_Contracts;

            --  Manager.Create_Asks;
            Manager.State := Bidding;
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
