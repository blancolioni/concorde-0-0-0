with Concorde.Agents;
with Concorde.Logging;
with Concorde.Logs;

package body Concorde.Markets is

   Recent_Trade_Limit : constant Duration :=
                          86_400.0;
   Recent_Offer_Limit : constant Duration :=
                          7.0 * 86_400.0;

   function Taxable_Image
     (Price_With_Tax : WL.Money.Price_Type;
      Tax_Rate       : Non_Negative_Real)
      return String
     with Unreferenced;

   function Get_Tax_Category
     (Market        : Root_Market_Type'Class;
      Buyer, Seller : not null access constant
        Concorde.Trades.Trader_Interface'Class;
      Commodity     : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Concorde.Trades.Market_Tax_Category
   is (if Buyer.Market_Resident
       then (if Seller.Market_Resident
             then Concorde.Trades.Sales
             else Concorde.Trades.Import)
       elsif Seller.Market_Resident
       then Concorde.Trades.Export
       else Concorde.Trades.Sales);

   -------------------------
   -- Add_Commodity_Offer --
   -------------------------

   procedure Add_Commodity_Offer
     (Info     : Cached_Commodity;
      Offer    : Concorde.Trades.Offer_Type;
      Quantity : WL.Quantities.Quantity_Type;
      Price    : WL.Money.Price_Type)
   is
      use WL.Quantities;
   begin
      while not Info.Recent_Offers.Is_Empty loop
         declare
            use Concorde.Calendar;
            Item : constant Offer_Record :=
                     Info.Recent_Offers.First_Element;
         begin
            if Clock - Item.Time_Stamp > Recent_Offer_Limit then
               case Item.Offer is
                  when Concorde.Trades.Ask =>
                     Info.Daily_Supply := Info.Daily_Supply
                       - Item.Quantity;
                  when Concorde.Trades.Bid =>
                     Info.Daily_Demand := Info.Daily_Demand
                       - Item.Quantity;
               end case;
               Info.Recent_Offers.Delete_First;
            else
               exit;
            end if;
         end;
      end loop;

      case Offer is
         when Concorde.Trades.Ask =>
            Info.Daily_Supply := Info.Daily_Supply + Quantity;
         when Concorde.Trades.Bid =>
            Info.Daily_Demand := Info.Daily_Demand + Quantity;
      end case;

      Info.Recent_Offers.Append
        (Offer_Record'
           (Time_Stamp => Concorde.Calendar.Clock,
            Offer      => Offer,
            Quantity   => Quantity,
            Price      => Price));
   end Add_Commodity_Offer;

   ---------------------
   -- Check_Commodity --
   ---------------------

   procedure Check_Commodity
     (Market    : Root_Market_Type'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
   is
   begin
      if Market.Commodities.Element (Commodity) = null then
         declare
            New_Cached : constant Cached_Commodity :=
                           new Cached_Commodity_Record'
                             (others => <>);
         begin
            New_Cached.Recent_Time := Concorde.Calendar.Clock;
            Market.Commodities.Replace_Element
              (Commodity, New_Cached);
         end;
      end if;
   end Check_Commodity;

   ------------------
   -- Check_Market --
   ------------------

   procedure Check_Market
     (Market : Root_Market_Type'Class)
   is
      use WL.Quantities;

      procedure Check
        (Commodity : Concorde.Commodities.Root_Commodity_Type'Class;
         Cache     : Cached_Commodity);

      -----------
      -- Check --
      -----------

      procedure Check
        (Commodity : Concorde.Commodities.Root_Commodity_Type'Class;
         Cache     : Cached_Commodity)
      is
      begin
         for Metric of Cache.Metrics loop
            if (for all Quantity of Metric.Quantities => Quantity = Zero) then
               Market.Log
                 (Concorde.Calendar.Image (Metric.Date, True)
                  & ": "
                  & Commodity.Name
                  & ": bad metric");
            end if;
         end loop;
      end Check;

   begin
      if False then
         Market.Log ("checking market");
         Market.Commodities.Scan (Check'Access);
      end if;
   end Check_Market;

   ------------------
   -- Check_Trades --
   ------------------

   procedure Check_Trades
     (Market    : Root_Market_Type'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
   is
      use WL.Money, WL.Quantities;
      Info : constant Cached_Commodity :=
               Market.Get_Commodity (Commodity);
      Bid_Offer    : Offer_Info;
      Ask_Offer    : Offer_Info;
      Bid_Price    : Price_Type := Zero;
      Bid_Quantity : Quantity_Type := Zero;
      Ask_Price    : Price_Type := Zero;
      Ask_Quantity : Quantity_Type := Zero;
      Hist         : Historical_Quantity_Record :=
                       (Concorde.Calendar.Clock, others => <>);

   begin

      loop
         if Bid_Quantity = Zero then
            exit when Info.Bids.Is_Empty;
            Bid_Offer := Info.Bids.Maximum_Element;
            Bid_Quantity := Bid_Offer.Remaining_Quantity;
            if not Commodity.Is_Set (Concorde.Commodities.Virtual) then
               Bid_Quantity :=
                 Min (Bid_Quantity,
                      Bid_Offer.Agent.Available_Capacity);
            end if;
            Bid_Price := Bid_Offer.Offer_Price;
         end if;

         if Ask_Quantity = Zero then
            exit when Info.Asks.Is_Empty;
            Ask_Offer := Info.Asks.Maximum_Element;
            Ask_Quantity := Ask_Offer.Remaining_Quantity;
            Ask_Price := Ask_Offer.Offer_Price;
         end if;

         exit when Bid_Price < Ask_Price;

         declare
            Final_Price         : constant Price_Type :=
                                    Adjust_Price (Ask_Price + Bid_Price,
                                                  0.5);
            Bidder_Cash         : constant Money_Type :=
                                    Bid_Offer.Agent.Limit_Cash;
            Affordable_Quantity : constant Quantity_Type :=
                                    (if Bidder_Cash > Zero
                                     then Get_Quantity
                                       (Bidder_Cash, Final_Price)
                                     else Zero);
            Traded_Quantity     : constant Quantity_Type :=
                                    Min (Affordable_Quantity,
                                         Min (Ask_Quantity, Bid_Quantity));
         begin

            Ask_Quantity := Ask_Quantity - Traded_Quantity;

            if Bid_Quantity > Affordable_Quantity then
               Bid_Offer.Agent.Log_Trade
                 ("limit cash: " & Image (Bid_Offer.Agent.Limit_Cash)
                  & " price " & Image (Final_Price)
                  & " affordable quantity " & Image (Affordable_Quantity));

               Bid_Quantity := Zero;
            else
               Bid_Quantity := Bid_Quantity - Traded_Quantity;
            end if;

            Hist.Quantities (Concorde.Trades.Total_Traded) :=
              Hist.Quantities (Concorde.Trades.Total_Traded)
              + Traded_Quantity;

            Info.Current_Supply := Info.Current_Supply - Traded_Quantity;
            Info.Current_Demand := Info.Current_Demand - Traded_Quantity;

            Market.Execute_Trade
              (Buyer     => Bid_Offer.Agent,
               Seller    => Ask_Offer.Agent,
               Commodity => Concorde.Commodities.Commodity_Type (Commodity),
               Quantity  => Traded_Quantity,
               Price     => Final_Price);

            Info.Historical_Mean_Price :=
              Adjust_Price
                (Info.Historical_Mean_Price + Final_Price, 0.5);

            if Ask_Quantity = Zero then
               Info.Asks.Delete_Maximum;
            end if;

            if Bid_Quantity = Zero then
               Info.Bids.Delete_Maximum;
            end if;
         end;

      end loop;

      if Ask_Quantity > Zero then
         Info.Asks.Delete_Maximum;
         Ask_Offer.Remaining_Quantity := Ask_Quantity;
         Info.Asks.Insert (Ask_Offer.Offer_Price, Ask_Offer);
      end if;

      if Bid_Quantity > Zero then
         Info.Bids.Delete_Maximum;
         Bid_Offer.Remaining_Quantity := Bid_Quantity;
         Info.Bids.Insert (Bid_Offer.Offer_Price, Bid_Offer);
      end if;

      declare
         L : Quantity_Metric_Lists.List renames
               Market.Get_Commodity (Commodity).Metrics;
      begin
         L.Insert (L.First, Hist);
      end;

   end Check_Trades;

   -------------------
   -- Create_Market --
   -------------------

   function Create_Market
     (Identifier : String;
      Owner      : not null access constant
        Concorde.Objects.Named_Object_Interface'Class;
      Manager : not null access constant
        Concorde.Trades.Trade_Manager_Interface'Class;
      Enable_Logging : Boolean)
      return Market_Type
   is

      procedure Create
        (Market : in out Root_Market_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create
        (Market : in out Root_Market_Type'Class)
      is
      begin
         Market.Id := new String'(Identifier);
         Market.Owner := Owner;
         Market.Manager := Manager;
         Market.Commodities := new Cached_Commodity_Vectors.Vector;
         Market.Enable_Logging := Enable_Logging;
      end Create;

   begin
      return Concorde.Markets.Db.Create (Create'Access);
   end Create_Market;

   ------------------
   -- Create_Offer --
   ------------------

   overriding procedure Create_Offer
     (Market    : Root_Market_Type;
      Offer     : Concorde.Trades.Offer_Type;
      Trader    : not null access constant
        Concorde.Trades.Trader_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : WL.Quantities.Quantity_Type;
      Price     : WL.Money.Price_Type)
   is
      use WL.Money;
      use WL.Quantities;
      Agent : constant Concorde.Agents.Agent_Type :=
                Concorde.Agents.Agent_Type (Trader);
      Info : constant Cached_Commodity :=
               Market.Get_Commodity (Commodity);
--        Hist      : Historical_Quantity_Record :=
--                      (Concorde.Calendar.Clock, others => <>);

      Offer_Name : constant String :=
                     (case Offer is
                         when Concorde.Trades.Ask =>
                            "ask",
                         when Concorde.Trades.Bid =>
                            "bid");

      Market_Log_Path  : constant String :=
                           Market.Identifier
                           & "/" & Commodity.Identifier
                           & "/" & Offer_Name;
   begin

      if Quantity = Zero then
         Market.Log ("error: quantity is zero");
      end if;

      pragma Assert (Quantity > Zero);

      Concorde.Logs.Log_Fields
        (Market_Log_Path,
         Trader.Short_Name,
         Market.Identifier,
         Commodity.Identifier,
         Image (Quantity),
         Image (Price));

      if Info.Historical_Mean_Price = Zero then
         Info.Historical_Mean_Price := Price;
      end if;

      case Offer is
         when Concorde.Trades.Bid =>
            Info.Bids.Insert
              (Price, Offer_Info'(Agent, Quantity, Quantity, Price));
            Info.Current_Demand := Info.Current_Demand + Quantity;
            --  Hist.Quantities (Concorde.Trades.Local_Demand) := Quantity;

         when Concorde.Trades.Ask =>

            Info.Asks.Insert
              (Price, Offer_Info'(Agent, Quantity, Quantity, Price));
            Info.Current_Supply := Info.Current_Supply + Quantity;
            --  Hist.Quantities (Concorde.Trades.Local_Supply) := Quantity;
      end case;

      Add_Commodity_Offer
        (Info     => Info,
         Offer    => Offer,
         Quantity => Quantity,
         Price    => Price);

--        if (for all X of Hist.Quantities => X = Zero) then
--           Market.Log (Commodity.Name & ": new metric: all zero");
--        end if;
--
--        declare
--           L : Quantity_Metric_Lists.List renames
--                 Market.Get_Commodity (Commodity).Metrics;
--        begin
--           L.Insert (L.First, Hist);
--           for H of L loop
--              if (for all X of H.Quantities => X = Zero) then
--                 Market.Log (Commodity.Name
--                             & ": error: all quanties are zero at "
--                             & Concorde.Calendar.Image (H.Date, True));
--              end if;
--           end loop;
--        end;

      Market.Check_Market;

      Market.Check_Trades (Commodity);

   end Create_Offer;

   --------------------
   -- Current_Demand --
   --------------------

   overriding function Current_Demand
     (Market    : Root_Market_Type;
      Item      : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return WL.Quantities.Quantity_Type
   is
   begin
      return Market.Get_Commodity (Item).Daily_Demand;
   end Current_Demand;

   -------------------
   -- Current_Price --
   -------------------

   overriding function Current_Price
     (Market    : Root_Market_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return WL.Money.Price_Type
   is
      use WL.Money, WL.Quantities;
      Info : constant Cached_Commodity :=
               Market.Get_Commodity (Commodity);
   begin
      if Info.Daily_Trade_Volume = Zero then
         if Info.Historical_Mean_Price = Zero then
            return Commodity.Base_Price;
         else
            return Info.Historical_Mean_Price;
         end if;
      else
         return Price (Info.Daily_Trade_Value,
                       Info.Daily_Trade_Volume);
      end if;
   end Current_Price;

   --------------------
   -- Current_Supply --
   --------------------

   overriding function Current_Supply
     (Market    : Root_Market_Type;
      Item      : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return WL.Quantities.Quantity_Type
   is
   begin
      return Market.Get_Commodity (Item).Daily_Supply;
   end Current_Supply;

   ------------------
   -- Delete_Offer --
   ------------------

   overriding procedure Delete_Offer
     (Market    : Root_Market_Type;
      Offer     : Concorde.Trades.Offer_Type;
      Trader    : Concorde.Trades.Trader_Interface'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
   is
      use WL.Money;
      Agent : Concorde.Agents.Root_Agent_Type'Class renames
                Concorde.Agents.Root_Agent_Type'Class (Trader);
      Info : constant Cached_Commodity :=
               Market.Get_Commodity (Commodity);

   begin

      case Offer is
         when Concorde.Trades.Bid =>
            declare
               New_Queue : Bid_Queues.Heap;
            begin
               while not Info.Bids.Is_Empty loop
                  declare
                     use type Concorde.Agents.Agent_Reference;
                     use type WL.Quantities.Quantity_Type;
                     Price : constant Price_Type :=
                               Info.Bids.Maximum_Key;
                     Element : constant Offer_Info :=
                                 Info.Bids.Maximum_Element;
                  begin
                     Info.Bids.Delete_Maximum;
                     if Element.Agent.Reference /= Agent.Reference then
                        New_Queue.Insert (Price, Element);
                     else
                        Agent.Log_Trade
                          ("deleting bid: "
                           & WL.Quantities.Image
                             (Element.Offered_Quantity)
                           & " " & Commodity.Identifier
                           & " @ " & Image (Element.Offer_Price));
                        Info.Current_Demand :=
                          Info.Current_Demand - Element.Remaining_Quantity;
                        Remove_Commodity_Offer
                          (Info    => Info,
                           Offer    => Offer,
                           Quantity => Element.Offered_Quantity,
                           Price    => Element.Offer_Price);
                     end if;
                  end;
               end loop;
               Info.Bids := New_Queue;
            end;
         when Concorde.Trades.Ask =>
            declare
               New_Queue : Ask_Queues.Heap;
            begin
               while not Info.Asks.Is_Empty loop
                  declare
                     use type Concorde.Agents.Agent_Reference;
                     use type WL.Quantities.Quantity_Type;
                     Price   : constant Price_Type :=
                                 Info.Asks.Maximum_Key;
                     Element : constant Offer_Info :=
                                 Info.Asks.Maximum_Element;
                  begin
                     Info.Asks.Delete_Maximum;
                     if Element.Agent.Reference /= Agent.Reference then
                        New_Queue.Insert (Price, Element);
                     else
                        Agent.Log_Trade
                          ("deleting ask: "
                           & WL.Quantities.Image
                             (Element.Offered_Quantity)
                           & " " & Commodity.Identifier
                           & " @ " & Image (Element.Offer_Price));
                        Info.Current_Supply :=
                          Info.Current_Supply - Element.Remaining_Quantity;
                        Remove_Commodity_Offer
                          (Info     => Info,
                           Offer    => Offer,
                           Quantity => Element.Offered_Quantity,
                           Price    => Element.Offer_Price);
                     end if;
                  end;
               end loop;
               Info.Asks := New_Queue;
            end;
      end case;
   end Delete_Offer;

   --------------------
   -- Enable_Logging --
   --------------------

   procedure Enable_Logging
     (Market  : in out Root_Market_Type'Class;
      Enabled : Boolean := True)
   is
   begin
      Market.Enable_Logging := Enabled;
   end Enable_Logging;

   -------------
   -- Execute --
   -------------

--     procedure Execute
--       (Market  : in out Root_Market_Type'Class;
--        Manager : not null access constant
--          Concorde.Trades.Trade_Manager_Interface'Class)
--     is
--
--        procedure Update_Commodity
--          (Commodity : Concorde.Commodities.Commodity_Type);
--
--        ----------------------
--        -- Update_Commodity --
--        ----------------------
--
--        procedure Update_Commodity
--          (Commodity : Concorde.Commodities.Commodity_Type)
--        is
--        begin
--           Market.Execute_Commodity_Trades (Manager, Commodity);
--        end Update_Commodity;
--
--        File : Ada.Text_IO.File_Type;
--
--     begin
--        if Market.Enable_Logging then
--           Ada.Text_IO.Create
--             (File, Ada.Text_IO.Out_File,
--              Concorde.Paths.Config_Path
--              & "/../log/markets/"
--              & Market.Name
--              & "-"
--              & Concorde.Calendar.Clock_To_String
--              & ".txt");
--           Ada.Text_IO.Set_Output (File);
--        end if;
--
--        for Commodity of Concorde.Commodities.All_Commodities loop
--           Update_Commodity (Commodity);
--        end loop;
--
--        if Market.Enable_Logging then
--           Ada.Text_IO.Set_Output
--             (Ada.Text_IO.Standard_Output);
--           Ada.Text_IO.Close (File);
--        end if;
--
--     exception
--        when E : others =>
--           Ada.Text_IO.Put_Line
--             ("exception: " & Ada.Exceptions.Exception_Message (E));
--           if Market.Enable_Logging then
--              Ada.Text_IO.Set_Output
--                (Ada.Text_IO.Standard_Output);
--              Ada.Text_IO.Close (File);
--           end if;
--
--           raise;
--     end Execute;

   -------------------
   -- Execute_Trade --
   -------------------

   procedure Execute_Trade
     (Market     : Root_Market_Type'Class;
      Buyer      : not null access constant
        Concorde.Trades.Trader_Interface'Class;
      Seller     : not null access constant
        Concorde.Trades.Trader_Interface'Class;
      Commodity  : Concorde.Commodities.Commodity_Type;
      Quantity   : WL.Quantities.Quantity_Type;
      Price      : WL.Money.Price_Type)
   is
      use Concorde.Commodities;
      use WL.Money;
      use WL.Quantities;
      Tax_Category   : constant Concorde.Trades.Market_Tax_Category :=
                         Get_Tax_Category (Market, Buyer, Seller, Commodity);
      Tax_Rate       : constant Unit_Real :=
                         Market.Manager.Tax_Rate (Tax_Category, Commodity);
      Taxed_Price    : constant Price_Type := Price;
      Tax_Free_Price : constant Price_Type :=
                         Without_Tax (Price, Float (Tax_Rate));
      Taxed_Cost     : constant Money_Type :=
                         Total (Price, Quantity);
      Tax_Free_Cost  : constant Money_Type :=
                         Without_Tax (Taxed_Cost, Float (Tax_Rate));
      Total_Tax      : constant Money_Type := Taxed_Cost - Tax_Free_Cost;

      Market_Log_Path  : constant String :=
                           Market.Identifier
                           & "/" & Commodity.Identifier
                           & "/transactions";
   begin
      Concorde.Logs.Log_Fields
        (Market_Log_Path,
         Buyer.Short_Name,
         Seller.Short_Name,
         Commodity.Identifier,
         WL.Quantities.Image (Quantity),
         Image (Tax_Free_Cost),
         Image (Total_Tax),
         Image (Taxed_Cost),
         Image (Tax_Free_Price),
         Image (Taxed_Price - Tax_Free_Price),
         Image (Price));

      Buyer.Execute_Trade
        (Offer     => Concorde.Trades.Bid,
         Commodity => Commodity,
         Quantity  => Quantity,
         Cost      => Taxed_Cost);

      Seller.Execute_Trade
        (Offer     => Concorde.Trades.Ask,
         Commodity => Commodity,
         Quantity  => Quantity,
         Cost      => Tax_Free_Cost);

      Market.Manager.Tax_Receipt
        (Commodity, Quantity, Price - Tax_Free_Price,
         Tax_Category, Total_Tax);

      declare
         Info : constant Cached_Commodity := Market.Get_Commodity (Commodity);
      begin
         while not Info.Recent_Transactions.Is_Empty loop
            declare
               use Concorde.Calendar;
               Item : constant Transaction_Record :=
                        Info.Recent_Transactions.First_Element;
            begin
               if Clock - Item.Time_Stamp > Recent_Trade_Limit then
                  Info.Daily_Trade_Value :=
                    Info.Daily_Trade_Value - Item.Cost;
                  Info.Daily_Trade_Volume :=
                    Info.Daily_Trade_Volume - Item.Quantity;
                  Info.Recent_Transactions.Delete_First;
               else
                  exit;
               end if;
            end;
         end loop;

         Info.Daily_Trade_Value :=
           Info.Daily_Trade_Value + Tax_Free_Cost;
         Info.Daily_Trade_Volume :=
           Info.Daily_Trade_Volume + Quantity;
         Info.Recent_Transactions.Append
           (Transaction_Record'
              (Time_Stamp => Concorde.Calendar.Clock,
               Quantity   => Quantity,
               Price      => Tax_Free_Price,
               Cost       => Tax_Free_Cost));
         Market.Log (Commodity.Name
                     & ": recent activity: volume = "
                     & Show (Info.Daily_Trade_Volume)
                     & "; value = "
                     & Show (Info.Daily_Trade_Value)
                     & "; ave price = "
                     & Show (WL.Money.Price (Info.Daily_Trade_Value,
                       Info.Daily_Trade_Volume)));
      end;

      if Commodity.Class = Skill then
         Buyer.Execute_Hire
           (Seller, Commodity, Quantity, Price);
      end if;

   end Execute_Trade;

   -------------------
   -- Get_Commodity --
   -------------------

   function Get_Commodity
     (Market    : Root_Market_Type'Class;
      Commodity : not null access constant
        Commodities.Root_Commodity_Type'Class)
      return Cached_Commodity
   is
   begin
      Market.Check_Commodity (Commodity);
      return Market.Commodities.Element (Commodity);
   end Get_Commodity;

--     ------------------
--     -- Get_Quantity --
--     ------------------
--
--     overriding function Get_Quantity
--       (Market    : Root_Market_Type;
--        Item      : not null access constant
--          Concorde.Commodities.Root_Commodity_Type'Class;
--        Metric    : Concorde.Trades.Trade_Metric;
--        Start     : Concorde.Calendar.Time;
--        Finish    : Concorde.Calendar.Time)
--        return WL.Quantities.Quantity_Type
--     is
--        use Concorde.Calendar;
--        use WL.Quantities;
--        use all type Concorde.Trades.Trade_Metric;
--        Market_Log_Path  : constant String :=
--                             Market.Identifier
--                             & "/" & Item.Identifier
--                             & "/" & Concorde.Trades.Metric_Id (Metric);
--        Result : Quantity_Type := Zero;
--        Cached : constant Cached_Commodity :=
--                   Market.Get_Commodity (Item);
--     begin
--        for Hist of Cached.Metrics loop
--           exit when Hist.Date < Start;
--           if Hist.Date <= Finish then
--              Result := Result + Hist.Quantities (Metric);
--           end if;
--        end loop;
--
--        if Metric = Local_Demand then
--           Concorde.Logs.Log_Fields
--             (Market_Log_Path,
--              Image (Start, True),
--              Image (Finish, True),
--              Item.Identifier,
--              Image (Result),
--              Image (Cached.Current_Demand),
--              Image (Result + Cached.Current_Demand));
--
--           Result := Result + Cached.Current_Demand;
--        elsif Metric = Local_Supply then
--           Concorde.Logs.Log_Fields
--             (Market_Log_Path,
--              Image (Start, True),
--              Image (Finish, True),
--              Item.Identifier,
--              Image (Result),
--              Image (Cached.Current_Supply),
--              Image (Result + Cached.Current_Supply));
--           Result := Result + Cached.Current_Supply;
--        end if;
--
--        return Result;
--     end Get_Quantity;

   ---------------------------
   -- Historical_Mean_Price --
   ---------------------------

   overriding function Historical_Mean_Price
     (Market    : Root_Market_Type;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return WL.Money.Price_Type
   is
      use WL.Money;
   begin
      return Price : WL.Money.Price_Type :=
        Market.Get_Commodity
          (Concorde.Commodities.Commodity_Type (Commodity))
            .Historical_Mean_Price
      do
         if Price = Zero then
            Price := Commodity.Base_Price;
         end if;
      end return;
   end Historical_Mean_Price;

   -------------------
   -- Initial_Price --
   -------------------

   procedure Initial_Price
     (Market    : in out Root_Market_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Price     : WL.Money.Price_Type)
   is
      Info : constant Cached_Commodity := Market.Get_Commodity (Commodity);
   begin
      Concorde.Logging.Log
        (Market.Name, Commodity.Name, "initial price",
         WL.Money.Image (Price));
      Info.Historical_Mean_Price := Price;
   end Initial_Price;

   ---------------
   -- Log_Offer --
   ---------------

   procedure Log_Offer
     (Market    : Root_Market_Type'Class;
      Message   : String;
      Commodity : Concorde.Commodities.Commodity_Type;
      Offer     : Offer_Info)
   is
      use WL.Money;
      use WL.Quantities;
   begin
      Market.Log
        (Message
         & ": "
         & Offer.Agent.Short_Name
         & ": "
         & Image (Offer.Remaining_Quantity)
         & " "
         & Commodity.Name
         & " @ "
         & Image (Offer.Offer_Price)
         & " each");
   end Log_Offer;

   ----------
   -- Name --
   ----------

   function Name
     (Market : Root_Market_Type'Class)
      return String
   is
   begin
      return Market.Owner.Name;
   end Name;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Market : Root_Market_Type)
      return Memor.Memor_Database
   is
      pragma Unreferenced (Market);
   begin
      return Db.Get_Database;
   end Object_Database;

   -----------
   -- Price --
   -----------

   overriding function Price
     (Market    : Root_Market_Type;
      Offer     : Concorde.Trades.Offer_Type;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      Quantity  : WL.Quantities.Quantity_Type)
      return WL.Money.Price_Type
   is
      pragma Unreferenced (Quantity);
      Info  : constant Cached_Commodity :=
                Market.Get_Commodity (Commodity);
   begin
      case Offer is
         when Concorde.Trades.Ask =>
            if not Info.Bids.Is_Empty then
               return Info.Bids.Maximum_Key;
            else
               return WL.Money.Zero;
            end if;
         when Concorde.Trades.Bid =>
            if not Info.Asks.Is_Empty then
               return Info.Asks.Maximum_Key;
            else
               return WL.Money.Zero;
            end if;
      end case;
   end Price;

   ----------------------------
   -- Remove_Commodity_Offer --
   ----------------------------

   procedure Remove_Commodity_Offer
     (Info     : Cached_Commodity;
      Offer    : Concorde.Trades.Offer_Type;
      Quantity : WL.Quantities.Quantity_Type;
      Price    : WL.Money.Price_Type)
   is
      use WL.Money;
      use WL.Quantities;
      Found : Recent_Offer_Lists.Cursor;
   begin
      for Position in Info.Recent_Offers.Iterate loop
         declare
            Item : constant Offer_Record :=
                     Recent_Offer_Lists.Element (Position);
         begin
            if Item.Quantity = Quantity
              and then Item.Price = Price
            then
               Found := Position;
               exit;
            end if;
         end;
      end loop;

      if Recent_Offer_Lists.Has_Element (Found) then
         Info.Recent_Offers.Delete (Found);
         case Offer is
            when Concorde.Trades.Ask =>
               Info.Daily_Supply := Info.Daily_Supply - Quantity;
            when Concorde.Trades.Bid =>
               Info.Daily_Demand := Info.Daily_Demand - Quantity;
         end case;
      end if;

   end Remove_Commodity_Offer;

   -------------------
   -- Taxable_Image --
   -------------------

   function Taxable_Image
     (Price_With_Tax : WL.Money.Price_Type;
      Tax_Rate       : Non_Negative_Real)
      return String
   is
      use WL.Money;
      Price_Without_Tax : constant Price_Type :=
                            Without_Tax (Price_With_Tax, Float (Tax_Rate));
      Tax               : constant Price_Type :=
                            Price_With_Tax
                              - Price_Without_Tax;
   begin
      return Image (Price_With_Tax)
        & " (" & Image (Price_Without_Tax)
        & " + " & Image (Tax) & ")";
   end Taxable_Image;

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant Root_Market_Type'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update;

   ------------------
   -- Update_Offer --
   ------------------

   overriding procedure Update_Offer
     (Market    : Root_Market_Type;
      Offer     : Concorde.Trades.Offer_Type;
      Trader    : Concorde.Trades.Trader_Interface'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      New_Price : WL.Money.Price_Type)
   is
      use WL.Money;
      Agent   : Concorde.Agents.Root_Agent_Type'Class renames
                  Concorde.Agents.Root_Agent_Type'Class (Trader);
      Info    : constant Cached_Commodity :=
                  Market.Get_Commodity (Commodity);
      Current : Offer_Info;
      Found   : Boolean := False;
   begin

      case Offer is
         when Concorde.Trades.Bid =>
            declare
               New_Queue : Bid_Queues.Heap;
            begin
               while not Info.Bids.Is_Empty loop
                  declare
                     use type Concorde.Agents.Agent_Reference;
                     use type WL.Quantities.Quantity_Type;
                     Price : constant Price_Type :=
                               Info.Bids.Maximum_Key;
                     Element : constant Offer_Info :=
                                 Info.Bids.Maximum_Element;
                  begin
                     Info.Bids.Delete_Maximum;
                     if Element.Agent.Reference /= Agent.Reference then
                        New_Queue.Insert (Price, Element);
                     else
                        Agent.Log_Trade
                          ("updating bid: "
                           & WL.Quantities.Image
                             (Element.Remaining_Quantity)
                           & " " & Commodity.Identifier
                           & " @ " & Image (Element.Offer_Price)
                           & ": new price " & Image (New_Price));
                        Current := Element;
                        Found := True;
                        Info.Current_Demand :=
                          Info.Current_Demand - Current.Remaining_Quantity;
                     end if;
                  end;
               end loop;
               Info.Bids := New_Queue;
            end;
         when Concorde.Trades.Ask =>
            declare
               use type Concorde.Agents.Agent_Reference;
               New_Queue : Ask_Queues.Heap;
            begin
               while not Info.Asks.Is_Empty loop
                  declare
                     use type WL.Quantities.Quantity_Type;
                     Price   : constant Price_Type :=
                                 Info.Asks.Maximum_Key;
                     Element : constant Offer_Info :=
                                 Info.Asks.Maximum_Element;
                  begin
                     Info.Asks.Delete_Maximum;
                     if Element.Agent.Reference /= Agent.Reference then
                        New_Queue.Insert (Price, Element);
                     else
                        Agent.Log_Trade
                          ("updating ask: "
                           & WL.Quantities.Image
                             (Element.Remaining_Quantity)
                           & " " & Commodity.Identifier
                           & " @ " & Image (Element.Offer_Price)
                           & ": new price " & Image (New_Price));
                        Current := Element;
                        Found := True;
                        Info.Current_Supply :=
                          Info.Current_Supply - Current.Remaining_Quantity;
                     end if;
                  end;
               end loop;
               Info.Asks := New_Queue;
            end;
      end case;

      if Found then
         Market.Create_Offer
           (Offer     => Offer,
            Trader    => Current.Agent,
            Commodity => Concorde.Commodities.Commodity_Type (Commodity),
            Quantity  => Current.Remaining_Quantity,
            Price     => New_Price);
      end if;

   end Update_Offer;

end Concorde.Markets;
