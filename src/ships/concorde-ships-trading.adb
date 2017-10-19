with WL.Money;

with Concorde.Worlds;

with Concorde.Commodities.Db;
with Concorde.Worlds.Db;

package body Concorde.Ships.Trading is

   procedure Create_Trade_Orders
     (Ship : not null access Concorde.Ships.Root_Ship_Type'Class);

   -------------------------
   -- Create_Trade_Orders --
   -------------------------

   procedure Create_Trade_Orders
     (Ship : not null access Concorde.Ships.Root_Ship_Type'Class)
   is

      From  : constant Concorde.Worlds.World_Type :=
                Concorde.Worlds.Db.Reference (Ship.Trade_From);
      To    : constant Concorde.Worlds.World_Type :=
                Concorde.Worlds.Db.Reference (Ship.Trade_To);

      procedure Check (Commodity : Concorde.Commodities.Commodity_Type);

      -----------
      -- Check --
      -----------

      procedure Check (Commodity : Concorde.Commodities.Commodity_Type) is
         use WL.Money;
         use WL.Quantities;
         Supply  : constant Quantity :=
                     From.Export_Market_Size (Commodity);
         Demand  : constant Quantity :=
                     To.Import_Market_Size (Commodity);
      begin
         if Supply > Zero and then Demand > Zero then
            declare
               Buy_At  : constant WL.Money.Price_Type :=
                           From.Sell_Price (Commodity);
               Sell_At : constant WL.Money.Price_Type :=
                           To.Buy_Price (Commodity);
            begin
               if Sell_At > Buy_At then
                  Ship.Log_Trade
                    (Commodity.Name
                     & ": supply "
                     & Image (Supply)
                     & " @ "
                     & Money.Image (Buy_At)
                     & "; demand at "
                     & To.Name
                     & " is "
                     & Image (Demand)
                     & " @ "
                     & Money.Image (Sell_At));
                  declare
                     Wanted : constant Quantity :=
                                Min (Ship.Hold_Quantity, Min (Supply, Demand));
                  begin
                     Ship.Buy_Requirements.Set_Quantity
                       (Commodity, Wanted, Total (Buy_At, Wanted));
                  end;
               end if;
            end;
         end if;
      end Check;

   begin
      Ship.Buy_Requirements.Clear_Stock;
      Concorde.Commodities.Db.Scan (Check'Access);
      Ship.Have_Trade_Orders := True;
   end Create_Trade_Orders;

   -----------
   -- Trade --
   -----------

   procedure Trade
     (Ship : not null access Concorde.Ships.Root_Ship_Type'Class)
   is

      use WL.Money;
      use WL.Quantities;

      Finished : Boolean := True;

      procedure Check_Buy (Commodity : Concorde.Commodities.Commodity_Type);

      procedure Check_Sell (Commodity : Concorde.Commodities.Commodity_Type);

      ---------------
      -- Check_Buy --
      ---------------

      procedure Check_Buy
        (Commodity : Concorde.Commodities.Commodity_Type)
      is
         From  : constant Concorde.Worlds.World_Type :=
                   Concorde.Worlds.Db.Reference (Ship.Trade_From);

         Wanted : constant Quantity :=
                    Ship.Buy_Requirements.Get_Quantity (Commodity);
         Have    : constant Quantity :=
                     Ship.Get_Quantity (Commodity);
         Price   : constant Price_Type :=
                     (if Wanted > Have
                      then From.Sell_Price (Commodity)
                      else Zero);
         Space   : constant Quantity :=
                     Ship.Hold_Quantity - Ship.Total_Quantity;
         Maximum : constant Quantity :=
                     (if Price > Zero
                      then WL.Money.Get_Quantity (Ship.Cash, Price)
                      else Zero);

         procedure Execute_Buy
           (World : in out Concorde.Worlds.Root_World_Type'Class);

         -----------------
         -- Execute_Buy --
         -----------------

         procedure Execute_Buy
           (World : in out Concorde.Worlds.Root_World_Type'Class)
         is
            Required : constant Quantity :=
                         Min (Space, Min (Wanted - Have, Maximum));
            Traded_Quantity : Quantity := Required;
            Cost            : constant Money_Type :=
                                Total (Price, Traded_Quantity);
         begin
            World.Sell (Commodity, Traded_Quantity);

            Ship.Execute_Trade
              (Offer     => Concorde.Trades.Buy,
               Commodity => Commodity,
               Quantity  => Traded_Quantity,
               Cost      => Cost);

            Ship.Log_Trade
              ("bought " & Image (Traded_Quantity)
               & "/" & Image (Required)
               & " "
               & Commodity.Name
               & " @ "
               & Image (Price)
               & " ea");
            if Traded_Quantity < Required then
               Finished := False;
            end if;

         end Execute_Buy;

      begin
         if Have < Wanted and then Maximum > Zero then
            Concorde.Worlds.Db.Update
              (Ship.Trade_From, Execute_Buy'Access);
         end if;

      end Check_Buy;

      ----------------
      -- Check_Sell --
      ----------------

      procedure Check_Sell
        (Commodity : Concorde.Commodities.Commodity_Type)
      is
         From  : constant Concorde.Worlds.World_Type :=
                   Concorde.Worlds.Db.Reference (Ship.Trade_From);

         Wanted    : constant Quantity :=
                       Ship.Buy_Requirements.Get_Quantity (Commodity);
         Have      : constant Quantity :=
                       Ship.Get_Quantity (Commodity);
         Price     : constant Price_Type :=
                       (if Wanted = Zero and then Have > Zero
                        then From.Buy_Price (Commodity)
                        else Zero);
         Available : constant Quantity :=
                       From.Import_Market_Size (Commodity);

         procedure Execute_Sell
           (World : in out Concorde.Worlds.Root_World_Type'Class);

         ------------------
         -- Execute_Sell --
         ------------------

         procedure Execute_Sell
           (World : in out Concorde.Worlds.Root_World_Type'Class)
         is
            Traded_Quantity : Quantity :=
                                Min (Have, Available);
            Cost            : constant Money_Type :=
                                Total (Price, Traded_Quantity);
         begin
            World.Buy (Commodity, Traded_Quantity);
            Ship.Execute_Trade
              (Offer     => Concorde.Trades.Sell,
               Commodity => Commodity,
               Quantity  => Traded_Quantity,
               Cost      => Cost);

            Ship.Log_Trade
              ("sold " & Image (Traded_Quantity)
               & "/" & Image (Min (Have, Available))
               & " "
               & Commodity.Name
               & " @ "
               & Image (Price)
               & " ea");

            if Ship.Get_Quantity (Commodity) > Zero then
               Finished := False;
            end if;
         end Execute_Sell;

      begin
         if Wanted = Zero and then Available > Zero
           and then Have > Zero and then Price > Zero
         then
            Concorde.Worlds.Db.Update
              (Ship.Trade_From, Execute_Sell'Access);
         end if;

      end Check_Sell;

   begin
      if not Ship.Is_Trader then
         return;
      end if;

      if not Ship.Orbiting
        (Concorde.Worlds.Db.Reference (Ship.Trade_From))
      then
         return;
      end if;

      if not Ship.Have_Trade_Orders then
         Create_Trade_Orders (Ship);
      end if;

      Ship.Scan_Stock (Check_Sell'Access);
      Ship.Buy_Requirements.Scan_Stock (Check_Buy'Access);

      if Finished then
         Ship.Log_Trade
           ("Finished trading at "
            & Concorde.Worlds.Db.Reference (Ship.Trade_From).Name
            & "; cash "
            & WL.Money.Image (Ship.Cash)
            & "; goods "
            & WL.Money.Image (Ship.Total_Value)
            & "; worth "
            & WL.Money.Image (Ship.Cash + Ship.Total_Value));

         declare
            Tmp : constant Memor.Database_Reference :=
                    Ship.Trade_From;
         begin
            Ship.Set_Destination
              (Concorde.Worlds.Db.Reference (Ship.Trade_To));
            Ship.Trade_From := Ship.Trade_To;
            Ship.Trade_To := Tmp;
         end;

         Ship.Have_Trade_Orders := False;

      end if;

   end Trade;

end Concorde.Ships.Trading;
