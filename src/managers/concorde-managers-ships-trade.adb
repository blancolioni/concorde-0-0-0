with WL.Heaps;
with Concorde.Money;
with Concorde.Quantities;

with Concorde.Locations;
with Concorde.Galaxy;

with Concorde.Objects.Queues;

package body Concorde.Managers.Ships.Trade is

   Detailed_Logging       : constant Boolean := True;

   function Buy_Exported_Commodities
     (Manager : in out Root_Ship_Trade_Manager'Class)
      return Boolean;

   function Buy_Exported_Commodities
     (Manager : in out Root_Ship_Trade_Manager'Class)
      return Boolean
   is

      use Concorde.Quantities;

      type Destination_Record is
         record
            Community   : Concorde.People.Communities.Community_Type;
            Commodities : Concorde.Commodities.Root_Stock_Type;
            Distance    : Non_Negative_Real;
            Profit      : Concorde.Money.Money_Type;
         end record;

      package Destination_Queues is
        new WL.Heaps (Real, Destination_Record);

      Queue : Destination_Queues.Heap;

      Current_System : constant Concorde.Systems.Star_System_Type :=
                         Manager.Community.World.System;

      Local_Exports  : Concorde.Commodities.Root_Stock_Type;

      Success        : Boolean := False;

      procedure Add_Local_Export
        (Commodity : Concorde.Commodities.Commodity_Type;
         Quantity  : Concorde.Quantities.Quantity_Type;
         Price     : Concorde.Money.Price_Type);

      procedure Check_Community
        (Community : Concorde.People.Communities.Community_Type);

      procedure Choose_Community;

      ----------------------
      -- Add_Local_Export --
      ----------------------

      procedure Add_Local_Export
        (Commodity : Concorde.Commodities.Commodity_Type;
         Quantity  : Concorde.Quantities.Quantity_Type;
         Price     : Concorde.Money.Price_Type)
      is
      begin
         Local_Exports.Add_Quantity
           (Commodity, Quantity,
            Concorde.Money.Total (Price, Quantity));
      end Add_Local_Export;

      ---------------------
      -- Check_Community --
      ---------------------

      procedure Check_Community
        (Community : Concorde.People.Communities.Community_Type)
      is
         use type Concorde.People.Communities.Community_Type;
      begin
         if Community /= Manager.Community then

            Manager.Ship.Log
              ("checking community: "
               & Community.Identifier);

            declare
               Rec : Destination_Record :=
                       (Community => Community,
                        Commodities => <>,
                        Distance    =>
                          Concorde.Galaxy.Shortest_Path_Distance
                            (Current_System,
                             Community.World.System),
                        Profit      => Concorde.Money.Zero);
               Score : Non_Negative_Real := 10.0 / Rec.Distance;

               procedure Add_Import
                 (Commodity : Concorde.Commodities.Commodity_Type;
                  Quantity  : Concorde.Quantities.Quantity_Type;
                  Price     : Concorde.Money.Price_Type);

               ----------------
               -- Add_Import --
               ----------------

               procedure Add_Import
                 (Commodity : Concorde.Commodities.Commodity_Type;
                  Quantity  : Concorde.Quantities.Quantity_Type;
                  Price     : Concorde.Money.Price_Type)
               is
                  use Concorde.Money;
                  Local_Quantity : constant Quantity_Type :=
                                     Local_Exports.Get_Quantity (Commodity);
                  Local_Price    : constant Price_Type :=
                                     Local_Exports.Get_Average_Price
                                       (Commodity);
                  Max_Quantity   : constant Quantity_Type :=
                                     Min
                                       (Manager.Ship.Available_Capacity,
                                        Min
                                          (Local_Quantity,
                                           Quantity));
               begin
                  Manager.Ship.Log
                    (Commodity.Identifier
                     & " "
                     & Show (Local_Quantity)
                     & "/"
                     & Show (Quantity)
                     & " for "
                     & Show (Local_Price)
                     & "/"
                     & Show (Price));

                  if Local_Quantity > Zero
                    and then Local_Price < Price
                  then
                     Rec.Commodities.Add_Quantity
                       (Commodity, Max_Quantity,
                        Concorde.Money.Total (Price, Max_Quantity));
                     Rec.Profit := Rec.Profit
                       + Total
                       (Price - Local_Price, Max_Quantity);
                  end if;
               end Add_Import;

            begin
               Rec.Commodities.Create_Virtual_Stock;
               Community.Scan_Imports (Add_Import'Access);
               Score := Score * Concorde.Money.To_Real (Rec.Profit);
               Queue.Insert (Score, Rec);
            end;
         end if;
      end Check_Community;

      ----------------------
      -- Choose_Community --
      ----------------------

      procedure Choose_Community is
         Destination : constant Destination_Record :=
                         Queue.First_Element;

         procedure Buy_Export
           (Commodity : Concorde.Commodities.Commodity_Type);

         ----------------
         -- Buy_Export --
         ----------------

         procedure Buy_Export
           (Commodity : Concorde.Commodities.Commodity_Type)
         is
            Bought : constant Concorde.Quantities.Quantity_Type :=
                       Concorde.Quantities.Min
                         (Destination.Commodities.Get_Quantity
                            (Commodity),
                          Manager.Ship.Available_Capacity);

            procedure Update
              (Ship : in out Concorde.Ships.Root_Ship_Type'Class);

            ------------
            -- Update --
            ------------

            procedure Update
              (Ship : in out Concorde.Ships.Root_Ship_Type'Class)
            is
            begin
               Manager.Community.Update.Buy_Export
                 (Buyer     => Ship,
                  Commodity => Commodity,
                  Quantity  => Bought);
            end Update;

         begin
            if Bought > Zero then
               Update (Manager.Ship.Update.all);
               Success := True;
            end if;
         end Buy_Export;

      begin
         Manager.Next_Destination := Destination.Community;
         Destination.Commodities.Scan_Stock
           (Buy_Export'Access);
      end Choose_Community;

   begin

      Local_Exports.Create_Virtual_Stock;
      Manager.Community.Scan_Exports (Add_Local_Export'Access);

      Concorde.People.Communities.Scan
        (Check_Community'Access);

      Choose_Community;

      return Success;

   end Buy_Exported_Commodities;

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
            if Buy_Exported_Commodities (Manager) then
               Manager.Ship.Log
                 ("leaving " & Manager.Community.Name);
               Manager.Community.Update.Remove_Ship (Manager.Ship);
               Manager.Community := null;
               Manager.Ship.Update.Clear_Wanted;
               Manager.Set_Destination (Manager.Next_Destination);
               Manager.State := Moving;
            end if;
         when Buying =>
            declare
               Available : constant Quantity_Type :=
                             Manager.Ship.Available_Capacity;
               Wanted    : constant Boolean :=
                             Manager.Ship.Has_Wanted;
            begin
               if Available = Zero or else not Wanted then
                  Manager.Ship.Log
                    ("leaving " & Manager.Community.Name);
                  Manager.Community.Update.Remove_Ship (Manager.Ship);
                  Manager.Community := null;
                  Manager.Ship.Update.Clear_Wanted;
                  Manager.Set_Destination (Manager.Next_Destination);
                  Manager.State := Moving;
               end if;
            end;
         when Moving =>
            Manager.State := Asking;
            Manager.Ship.Log
              ("arriving " & Manager.Next_Destination.Name);
            Manager.Next_Destination.Update.Add_Ship (Manager.Ship);
            Manager.Community := Manager.Next_Destination;
            Manager.Next_Destination := null;

         when Asking =>
            Manager.State := Selling;

         when Selling =>

            declare
               procedure Process_Commodity
                 (Commodity : Concorde.Commodities.Commodity_Type);

               -----------------------
               -- Process_Commodity --
               -----------------------

               procedure Process_Commodity
                 (Commodity : Concorde.Commodities.Commodity_Type)
               is
               begin
                  Manager.Community.Update.Sell_Import
                    (Seller    => Manager.Ship.Update.all,
                     Commodity => Commodity,
                     Quantity  => Manager.Ship.Get_Quantity (Commodity));
               end Process_Commodity;

            begin
               Manager.Ship.Scan_Stock (Process_Commodity'Access);
               if Manager.Ship.Total_Quantity = Zero then
                  Manager.State := Bidding;
               end if;
            end;
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
