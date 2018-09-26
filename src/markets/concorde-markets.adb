with Concorde.Elementary_Functions;

with Concorde.Agents;
with Concorde.Logging;
with Concorde.Logs;

with Concorde.Agents.Reports;

package body Concorde.Markets is

   Log_Market_Queue : constant Boolean := False;

   Recent_Trade_Limit : constant Duration :=
                          86_400.0;
   Recent_Offer_Limit : constant Duration :=
                          7.0 * 86_400.0;

   function Taxable_Image
     (Price_With_Tax : Concorde.Money.Price_Type;
      Tax_Rate       : Non_Negative_Real)
      return String
     with Unreferenced;

   function Get_Tax_Category
     (Market        : Root_Market_Type'Class;
      Buyer, Seller : not null access constant
        Concorde.Trades.Trader_Interface'Class;
      Commodity     : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Concorde.Trades.Market_Tax_Category;

   procedure Remove_Quantity
     (Info     : Cached_Commodity;
      Bid      : Concorde.Trades.Offer_Type;
      Resident : Boolean;
      Quantity : Concorde.Quantities.Quantity_Type);

   procedure Add_Quantity
     (Info     : Cached_Commodity;
      Bid      : Concorde.Trades.Offer_Type;
      Resident : Boolean;
      Quantity : Concorde.Quantities.Quantity_Type);

   procedure Remove_Old_Offers
     (Info : Cached_Commodity);

   procedure Remove_Old_Transactions
     (Info : Cached_Commodity);

   -------------------------
   -- Add_Commodity_Offer --
   -------------------------

   procedure Add_Commodity_Offer
     (Info     : Cached_Commodity;
      Offer    : Concorde.Trades.Offer_Type;
      Resident : Boolean;
      Quantity : Concorde.Quantities.Quantity_Type;
      Price    : Concorde.Money.Price_Type)
   is
   begin
      Remove_Old_Offers (Info);
      Add_Quantity (Info, Offer, Resident, Quantity);

      Info.Recent_Offers.Append
        (Offer_Record'
           (Time_Stamp => Concorde.Calendar.Clock,
            Offer      => Offer,
            Resident   => Resident,
            Quantity   => Quantity,
            Price      => Price));
   end Add_Commodity_Offer;

   ------------------
   -- Add_Quantity --
   ------------------

   overriding procedure Add_Quantity
     (Market    : Root_Market_Type;
      Metric    : Concorde.Trades.Quantity_Metric;
      Item      : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      Quantity  : Concorde.Quantities.Quantity_Type)
   is
      Info : constant Cached_Commodity :=
               Market.Get_Commodity (Item);
   begin
      Info.Quantity_Metrics (Metric) :=
        Info.Quantity_Metrics (Metric) + Quantity;
   end Add_Quantity;

   ------------------
   -- Add_Quantity --
   ------------------

   procedure Add_Quantity
     (Info     : Cached_Commodity;
      Bid      : Concorde.Trades.Offer_Type;
      Resident : Boolean;
      Quantity : Concorde.Quantities.Quantity_Type)
   is
      use Concorde.Quantities;
      use Concorde.Trades;
   begin
      case Bid is
         when Concorde.Trades.Ask =>
            Info.Quantity_Metrics (Current_Supply) :=
              Info.Quantity_Metrics (Current_Supply) + Quantity;
            if not Resident then
               Info.Quantity_Metrics (Current_Imports) :=
                 Info.Quantity_Metrics (Current_Imports) + Quantity;
            end if;

         when Concorde.Trades.Bid =>
            Info.Quantity_Metrics (Current_Demand) :=
              Info.Quantity_Metrics (Current_Demand) + Quantity;
            if not Resident then
               Info.Quantity_Metrics (Current_Exports) :=
                 Info.Quantity_Metrics (Current_Exports) + Quantity;
            end if;
      end case;
   end Add_Quantity;

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
   -- Check_Trades --
   ------------------

   procedure Check_Trades
     (Market    : Root_Market_Type'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
   is
      use Concorde.Money, Concorde.Quantities;
      Info : constant Cached_Commodity :=
               Market.Get_Commodity (Commodity);
      Bid_Offer    : Offer_Info;
      Ask_Offer    : Offer_Info;
      Bid_Price    : Price_Type := Zero;
      Bid_Quantity : Quantity_Type := Zero;
      Ask_Price    : Price_Type := Zero;
      Ask_Quantity : Quantity_Type := Zero;
   begin

      loop
         if Bid_Quantity = Zero then
            exit when Info.Bids.Is_Empty;
            Bid_Offer := Info.Bids.First_Element;
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
            Ask_Offer := Info.Asks.First_Element;
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
               Info.Asks.Delete_First;
            end if;

            if Bid_Quantity = Zero then
               Info.Bids.Delete_First;
            end if;
         end;

      end loop;

      if Ask_Quantity > Zero then
         Info.Asks.Delete_First;
         Ask_Offer.Remaining_Quantity := Ask_Quantity;
         Info.Asks.Insert (Ask_Offer.Offer_Price, Ask_Offer);
      end if;

      if Bid_Quantity > Zero then
         Info.Bids.Delete_First;
         Bid_Offer.Remaining_Quantity := Bid_Quantity;
         Info.Bids.Insert (Bid_Offer.Offer_Price, Bid_Offer);
      end if;

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
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type)
   is
      use Concorde.Money;
      use Concorde.Quantities;
      Agent : constant Concorde.Agents.Agent_Type :=
                Concorde.Agents.Agent_Type (Trader);
      Info : constant Cached_Commodity :=
                Market.Get_Commodity (Commodity);

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
         Resident => Trader.Market_Resident,
         Quantity => Quantity,
         Price    => Price);

      Market.Check_Trades (Commodity);

   end Create_Offer;

   -------------------
   -- Current_Price --
   -------------------

   overriding function Current_Price
     (Market    : Root_Market_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type
   is
      use Concorde.Money, Concorde.Quantities;
      Info : constant Cached_Commodity :=
               Market.Get_Commodity (Commodity);
   begin
      if Info.Recent_Trade_Volume = Zero then
         if Info.Historical_Mean_Price = Zero then
            return Commodity.Base_Price;
         else
            return Info.Historical_Mean_Price;
         end if;
      else
         return Price (Info.Recent_Trade_Value,
                       Info.Recent_Trade_Volume);
      end if;
   end Current_Price;

   ----------------------
   -- Current_Quantity --
   ----------------------

   overriding function Current_Quantity
     (Market    : Root_Market_Type;
      Metric    : Concorde.Trades.Quantity_Metric;
      Item      : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Market.Get_Commodity (Item).Quantity_Metrics (Metric);
   end Current_Quantity;

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
      use Concorde.Money;
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
                     Price : constant Price_Type :=
                               Info.Bids.First_Key;
                     Element : constant Offer_Info :=
                                 Info.Bids.First_Element;
                  begin
                     Info.Bids.Delete_First;
                     if Element.Agent.Reference /= Agent.Reference then
                        New_Queue.Insert (Price, Element);
                     else
                        Agent.Log_Trade
                          ("deleting bid: "
                           & Concorde.Quantities.Image
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
                     Price   : constant Price_Type :=
                                 Info.Asks.First_Key;
                     Element : constant Offer_Info :=
                                 Info.Asks.First_Element;
                  begin
                     Info.Asks.Delete_First;
                     if Element.Agent.Reference /= Agent.Reference then
                        New_Queue.Insert (Price, Element);
                     else
                        Agent.Log_Trade
                          ("deleting ask: "
                           & Concorde.Quantities.Image
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
      Quantity   : Concorde.Quantities.Quantity_Type;
      Price      : Concorde.Money.Price_Type)
   is
      use Concorde.Commodities;
      use Concorde.Money;
      use Concorde.Quantities;
      Tax_Category   : constant Concorde.Trades.Market_Tax_Category :=
                         Get_Tax_Category (Market, Buyer, Seller, Commodity);
      Tax_Rate       : constant Unit_Real :=
                         Market.Manager.Tax_Rate (Tax_Category, Commodity);
      Taxed_Price    : constant Price_Type := Price;
      Tax_Free_Price : constant Price_Type :=
                         Without_Tax (Price, Tax_Rate);
      Taxed_Cost     : constant Money_Type :=
                         Total (Price, Quantity);
      Tax_Free_Cost  : constant Money_Type :=
                         Without_Tax (Taxed_Cost, Tax_Rate);
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
         Concorde.Quantities.Image (Quantity),
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
         Remove_Old_Transactions (Info);

         Info.Recent_Trade_Value :=
           Info.Recent_Trade_Value + Tax_Free_Cost;
         Info.Recent_Trade_Volume :=
           Info.Recent_Trade_Volume + Quantity;

         Info.Recent_Transactions.Append
           (Transaction_Record'
              (Time_Stamp => Concorde.Calendar.Clock,
               Quantity   => Quantity,
               Price      => Tax_Free_Price,
               Cost       => Tax_Free_Cost));

         Market.Log (Commodity.Name
                     & ": recent activity: volume = "
                     & Show (Info.Recent_Trade_Volume)
                     & "; value = "
                     & Show (Info.Recent_Trade_Value)
                     & "; ave price = "
                     & Show
                       (Concorde.Money.Price
                          (Info.Recent_Trade_Value,
                           Info.Recent_Trade_Volume)));
      end;

--        if Commodity.Class = Skill then
--           Buyer.Execute_Hire
--             (Seller, Commodity, Quantity, Price);
--        end if;

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
--        return Concorde.Quantities.Quantity_Type
--     is
--        use Concorde.Calendar;
--        use Concorde.Quantities;
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

   ----------------------
   -- Get_Tax_Category --
   ----------------------

   function Get_Tax_Category
     (Market        : Root_Market_Type'Class;
      Buyer, Seller : not null access constant
        Concorde.Trades.Trader_Interface'Class;
      Commodity     : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Concorde.Trades.Market_Tax_Category
   is
      pragma Unreferenced (Market, Commodity);
   begin
      if Buyer.Market_Resident then
         if Seller.Market_Resident then
            return Concorde.Trades.Sales;
         else
            return Concorde.Trades.Import;
         end if;
      else
         if Seller.Market_Resident then
            return Concorde.Trades.Export;
         else
            return Concorde.Trades.Sales;
         end if;
      end if;
   end Get_Tax_Category;

   ---------------------------
   -- Historical_Mean_Price --
   ---------------------------

   overriding function Historical_Mean_Price
     (Market    : Root_Market_Type;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Concorde.Money.Price_Type
   is
      use Concorde.Money;
   begin
      return Price : Concorde.Money.Price_Type :=
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
      Price     : Concorde.Money.Price_Type)
   is
      Info : constant Cached_Commodity := Market.Get_Commodity (Commodity);
   begin
      Concorde.Logging.Log
        (Market.Name, Commodity.Name, "initial price",
         Concorde.Money.Image (Price));
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
      use Concorde.Money;
      use Concorde.Quantities;
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

   --------------------------
   -- Notify_Foreign_Trade --
   --------------------------

   overriding procedure Notify_Foreign_Trade
     (Market    : Root_Market_Type;
      Offer     : Concorde.Trades.Offer_Type;
      Trader    : not null access constant
        Concorde.Trades.Trader_Interface'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type)
   is
      use Concorde.Money;
      use Concorde.Quantities;
      Info  : constant Cached_Commodity :=
                Market.Get_Commodity (Commodity);

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

      Add_Commodity_Offer
        (Info     => Info,
         Offer    => Offer,
         Resident => Trader.Market_Resident,
         Quantity => Quantity,
         Price    => Price);

   end Notify_Foreign_Trade;

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
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Price_Type
   is
      pragma Unreferenced (Quantity);
      Info  : constant Cached_Commodity :=
                Market.Get_Commodity (Commodity);
   begin
      case Offer is
         when Concorde.Trades.Ask =>
            if not Info.Bids.Is_Empty then
               return Info.Bids.First_Key;
            else
               return Concorde.Money.Zero;
            end if;
         when Concorde.Trades.Bid =>
            if not Info.Asks.Is_Empty then
               return Info.Asks.First_Key;
            else
               return Concorde.Money.Zero;
            end if;
      end case;
   end Price;

   ------------------------------
   -- Recent_Transaction_Count --
   ------------------------------

   function Recent_Transaction_Count
     (Market    : Root_Market_Type'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Natural
   is
   begin
      return Natural
        (Market.Get_Commodity (Commodity).Recent_Transactions.Length);
   end Recent_Transaction_Count;

   ----------------------------
   -- Remove_Commodity_Offer --
   ----------------------------

   procedure Remove_Commodity_Offer
     (Info     : Cached_Commodity;
      Offer    : Concorde.Trades.Offer_Type;
      Quantity : Concorde.Quantities.Quantity_Type;
      Price    : Concorde.Money.Price_Type)
   is
      use Concorde.Money;
      use Concorde.Quantities;
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
         Remove_Quantity
           (Info, Offer,
            Recent_Offer_Lists.Element (Found).Resident,
            Recent_Offer_Lists.Element (Found).Quantity);
         Info.Recent_Offers.Delete (Found);

--           case Offer is
--              when Concorde.Trades.Ask =>
--                 Info.Daily_Supply := Info.Daily_Supply - Quantity;
--              when Concorde.Trades.Bid =>
--                 Info.Daily_Demand := Info.Daily_Demand - Quantity;
--           end case;
      end if;

   end Remove_Commodity_Offer;

   -----------------------
   -- Remove_Old_Offers --
   -----------------------

   procedure Remove_Old_Offers
     (Info : Cached_Commodity)
   is
      use type Concorde.Calendar.Time;
      Clock : constant Concorde.Calendar.Time :=
                Concorde.Calendar.Clock;
   begin
      while not Info.Recent_Offers.Is_Empty loop
         declare
            use Concorde.Calendar;
            Item : constant Offer_Record :=
                     Info.Recent_Offers.First_Element;
         begin
            if Clock - Item.Time_Stamp > Recent_Offer_Limit then
               Remove_Quantity (Info, Item.Offer, Item.Resident,
                                Item.Quantity);
               Info.Recent_Offers.Delete_First;
            else
               exit;
            end if;
         end;
      end loop;
   end Remove_Old_Offers;

   -----------------------------
   -- Remove_Old_Transactions --
   -----------------------------

   procedure Remove_Old_Transactions
     (Info : Cached_Commodity)
   is
      use type Concorde.Calendar.Time;
      Clock : constant Concorde.Calendar.Time :=
                Concorde.Calendar.Clock;
   begin
      while not Info.Recent_Transactions.Is_Empty loop
         declare
            use Concorde.Calendar;
            use Concorde.Money;
            Item : constant Transaction_Record :=
                     Info.Recent_Transactions.First_Element;
         begin
            if Clock - Item.Time_Stamp > Recent_Trade_Limit then
               Info.Recent_Trade_Value :=
                 Info.Recent_Trade_Value - Item.Cost;
               Info.Recent_Trade_Volume :=
                 Info.Recent_Trade_Volume - Item.Quantity;
               Info.Recent_Transactions.Delete_First;
            else
               exit;
            end if;
         end;
      end loop;
   end Remove_Old_Transactions;

   ---------------------
   -- Remove_Quantity --
   ---------------------

   procedure Remove_Quantity
     (Info     : Cached_Commodity;
      Bid      : Concorde.Trades.Offer_Type;
      Resident : Boolean;
      Quantity : Concorde.Quantities.Quantity_Type)
   is
      use Concorde.Quantities;
      use Concorde.Trades;
   begin
      case Bid is
         when Concorde.Trades.Ask =>
            Info.Quantity_Metrics (Current_Supply) :=
              Info.Quantity_Metrics (Current_Supply) - Quantity;
            if not Resident then
               Info.Quantity_Metrics (Current_Imports) :=
                 Info.Quantity_Metrics (Current_Imports) - Quantity;
            end if;

         when Concorde.Trades.Bid =>
            Info.Quantity_Metrics (Current_Demand) :=
              Info.Quantity_Metrics (Current_Demand) - Quantity;
            if not Resident then
               Info.Quantity_Metrics (Current_Exports) :=
                 Info.Quantity_Metrics (Current_Exports) - Quantity;
            end if;
      end case;
   end Remove_Quantity;

   -------------------
   -- Taxable_Image --
   -------------------

   function Taxable_Image
     (Price_With_Tax : Concorde.Money.Price_Type;
      Tax_Rate       : Non_Negative_Real)
      return String
   is
      use Concorde.Money;
      Price_Without_Tax : constant Price_Type :=
                            Without_Tax (Price_With_Tax, Tax_Rate);
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

   -------------------
   -- Update_Market --
   -------------------

   procedure Update_Market
     (Market : in out Market_Interface'Class)
   is
      procedure Update_Commodity
        (Commodity : Concorde.Commodities.Commodity_Type);

      ----------------------
      -- Update_Commodity --
      ----------------------

      procedure Update_Commodity
        (Commodity : Concorde.Commodities.Commodity_Type)
      is

         use Concorde.Quantities;
         use Concorde.Money;

         Bids : Offer_Queues.Heap;
         Asks : Offer_Queues.Heap;

         Total_Demand    : Quantity_Type := Zero;
         Total_Supply    : Quantity_Type := Zero;
         Total_Desire    : Quantity_Type := Zero;
         Total_Available : Quantity_Type := Zero;

         Price         : constant Price_Type :=
                           Market.Current_Price (Commodity);

         procedure Add_Agent_Needs
           (Agent : not null access constant
              Concorde.Agents.Root_Agent_Type'Class);

         procedure Resize_Offers
           (Large_Queue     : in out Offer_Queues.Heap;
            Large_Quantity  : Quantity_Type;
            Small_Quantity  : Quantity_Type);

         ---------------------
         -- Add_Agent_Needs --
         ---------------------

         procedure Add_Agent_Needs
           (Agent : not null access constant
              Concorde.Agents.Root_Agent_Type'Class)
         is
            Required      : constant Quantity_Type :=
                              Agent.Daily_Needs (Commodity);
            Supply        : constant Quantity_Type :=
                              Agent.Daily_Supply (Commodity);
            Have          : constant Quantity_Type :=
                              Agent.Get_Quantity (Commodity);
            Demand        : constant Quantity_Type :=
                              (if Have >= Required
                               then Zero else Required - Have);
            Budget        : constant Money_Type :=
                              Agent.Daily_Budget (Commodity);
            Bid           : constant Quantity_Type :=
                              Min (Demand, Get_Quantity (Budget, Price));
            Ask           : constant Quantity_Type := Supply;
         begin
            if Supply > Zero or else Bid > Zero or else Ask > Zero then
               Agent.Log (Commodity.Identifier
                          & ": want "
                          & Show (Required)
                          & "; have "
                          & Show (Have)
                          & "; budget "
                          & Show (Budget)
                          & "; bid "
                          & Show (Bid)
                          & "; ask "
                          & Show (Ask));
            end if;

            if Bid > Zero then
               Bids.Insert (Bid,
                            Offer_Info'
                              (Agent              => Agent,
                               Offered_Quantity   => Bid,
                               Remaining_Quantity => Bid,
                               Offer_Price        => Price));
               Total_Demand := Total_Demand + Bid;
            elsif Ask > Zero then
               Asks.Insert
                 (Key     => Ask,
                  Element =>
                    Offer_Info'
                      (Agent              => Agent,
                       Offered_Quantity   => Ask,
                       Remaining_Quantity => Ask,
                       Offer_Price        => Price));
               Total_Supply := Total_Supply + Ask;
            end if;

            if Required > Have then
               Total_Desire := Total_Desire + Demand;
            end if;

            Total_Available := Total_Available + Ask;
         end Add_Agent_Needs;

         -------------------
         -- Resize_Offers --
         -------------------

         procedure Resize_Offers
           (Large_Queue     : in out Offer_Queues.Heap;
            Large_Quantity  : Quantity_Type;
            Small_Quantity  : Quantity_Type)
         is
            New_Queue : Offer_Queues.Heap;
            Traded    : Quantity_Type := Zero;
         begin

            if Log_Market_Queue then
               Concorde.Logging.Log
                 (Actor    => "market",
                  Location => "",
                  Category => "resize",
                  Message  =>
                    "large queue length:"
                  & Natural'Image (Large_Queue.Length)
                  & "; large quantity: "
                  & Show (Large_Quantity)
                  & "; small quantity: "
                  & Show (Small_Quantity));
            end if;

            if Large_Queue.Length >= To_Natural (Small_Quantity) then
               for I in 1 .. To_Natural (Small_Quantity) loop
                  if Log_Market_Queue then
                     Large_Queue.First_Element.Agent.Log
                       ("changes from "
                        & Show (Large_Queue.First_Element.Offered_Quantity)
                        & " to 1");
                  end if;

                  New_Queue.Insert
                    (Unit,
                     (Large_Queue.First_Element with delta
                          Offered_Quantity => Unit));
                  Large_Queue.Delete_First;
               end loop;
            else
               while not Large_Queue.Is_Empty loop
                  declare
                     Top : constant Offer_Info := Large_Queue.First_Element;
                  begin
                     if Traded + Top.Offered_Quantity
                       < Scale (Small_Quantity, 0.5)
                     then
                        if Log_Market_Queue then
                           Large_Queue.First_Element.Agent.Log
                             ("gets "
                              & Show
                                (Large_Queue.First_Element.Offered_Quantity));
                        end if;

                        New_Queue.Insert (Top.Offered_Quantity, Top);
                        Large_Queue.Delete_First;
                        Traded := Traded + Top.Offered_Quantity;
                     else
                        exit;
                     end if;
                  end;
               end loop;

               declare
                  Factor : constant Unit_Real :=
                             To_Real (Small_Quantity - Traded)
                             / To_Real (Large_Quantity - Traded);
               begin
                  while not Large_Queue.Is_Empty loop
                     declare
                        Top : Offer_Info := Large_Queue.First_Element;
                        Q   : constant Quantity_Type :=
                                Top.Offered_Quantity;
                     begin
                        Large_Queue.Delete_First;
                        Top.Offered_Quantity :=
                          Min (Scale (Top.Offered_Quantity, Factor),
                               Small_Quantity - Traded);
                        if Log_Market_Queue then
                           Top.Agent.Log
                             ("changes from "
                              & Show (Q)
                              & " to "
                              & Show (Top.Offered_Quantity));
                        end if;

                        New_Queue.Insert (Top.Offered_Quantity, Top);
                        Traded := Traded + Top.Offered_Quantity;
                     end;
                  end loop;
               end;
            end if;

            Large_Queue := New_Queue;
         end Resize_Offers;

      begin

         Market.Scan_Agents (Add_Agent_Needs'Access);

         if True then
            Concorde.Logging.Log
              (Actor    => Market.Identifier,
               Location => "",
               Category => Commodity.Identifier,
               Message  =>
                 "price="
               & Concorde.Money.Show (Price)
               & "; supply=" & Concorde.Quantities.Show (Total_Supply)
               & "; demand=" & Concorde.Quantities.Show (Total_Demand)
               & "; available=" & Concorde.Quantities.Show (Total_Available)
               & "; desire="  & Concorde.Quantities.Show (Total_Desire));
         end if;

         if Total_Demand > Zero and then Total_Supply > Zero then
            if Total_Demand > Total_Supply then
               Resize_Offers (Bids, Total_Demand, Total_Supply);
            elsif Total_Supply > Total_Demand then
               Resize_Offers (Asks, Total_Supply, Total_Demand);
            end if;

            while not Bids.Is_Empty loop
               declare
                  Offer : constant Offer_Info := Bids.First_Element;
               begin
                  Bids.Delete_First;
                  Offer.Agent.Variable_Reference.On_Commodity_Buy
                    (Commodity, Offer.Offered_Quantity, Price);
               end;
            end loop;

            while not Asks.Is_Empty loop
               declare
                  Offer : constant Offer_Info := Asks.First_Element;
               begin
                  Asks.Delete_First;
                  Offer.Agent.Variable_Reference.On_Commodity_Sell
                    (Commodity, Offer.Offered_Quantity, Price);
               end;
            end loop;

         end if;

         declare
            New_Price : Price_Type := Price;
         begin
            if Total_Supply > Total_Demand
              and then Total_Desire > Zero
            then
               declare
                  Scale_Factor  : constant Non_Negative_Real :=
                                    (if Total_Demand = Zero
                                     then 1.0
                                     else To_Real (Total_Supply)
                                     / To_Real (Total_Demand)
                                     / 10.0);
                  Price_Factor  : constant Non_Negative_Real :=
                                    Concorde.Money.To_Real
                                      (Commodity.Base_Price);
                  Price_Change  : constant Non_Negative_Real :=
                                    Real'Min
                                      (Real'Max
                                         (Scale_Factor * Price_Factor * 0.01,
                                          0.01),
                                       To_Real (Price) * 0.2);
               begin
                  New_Price :=
                    To_Price
                      (Real'Max (Price_Factor / 10.0,
                       To_Real (Price) - Price_Change));
               end;

               Concorde.Logging.Log
                 (Actor    => "market",
                  Location => "",
                  Category => Commodity.Identifier,
                  Message  => "new price: "
                  & Concorde.Money.Show (New_Price));

            end if;

            if Total_Demand > Total_Supply
              and then Total_Available > Zero
            then
               declare
                  Supply_Factor : constant Unit_Real :=
                                    To_Real (Total_Supply)
                                    / To_Real (Total_Demand);
                  Scale_Factor  : constant Unit_Real :=
                                    (if Total_Supply = Zero
                                     then 1.0
                                     else Concorde.Elementary_Functions.Tanh
                                       (10.0 / Supply_Factor));
                  Price_Factor  : constant Non_Negative_Real :=
                                    Concorde.Money.To_Real
                                      (Commodity.Base_Price);
                  Price_Change  : constant Non_Negative_Real :=
                                    Scale_Factor
                                      * Price_Factor / 10.0;
               begin
                  New_Price := To_Price (To_Real (Price) + Price_Change);
               end;

               Concorde.Logging.Log
                 (Actor    => "market",
                  Location => "",
                  Category => Commodity.Identifier,
                  Message  => "new price: "
                  & Concorde.Money.Show (New_Price));
            end if;

            Market.Update_Commodity
              (Item      => Commodity,
               Demand    => Total_Demand,
               Supply    => Total_Supply,
               Available => Total_Available,
               Price     => New_Price);
         end;

      end Update_Commodity;

   begin
      Concorde.Commodities.Scan (Update_Commodity'Access);
      Concorde.Agents.Reports.Log_Status (Market);
   end Update_Market;

   ------------------
   -- Update_Offer --
   ------------------

   overriding procedure Update_Offer
     (Market    : Root_Market_Type;
      Offer     : Concorde.Trades.Offer_Type;
      Trader    : Concorde.Trades.Trader_Interface'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      New_Price : Concorde.Money.Price_Type)
   is
      use Concorde.Money;
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
                     Price : constant Price_Type :=
                               Info.Bids.First_Key;
                     Element : constant Offer_Info :=
                                 Info.Bids.First_Element;
                  begin
                     Info.Bids.Delete_First;
                     if Element.Agent.Reference /= Agent.Reference then
                        New_Queue.Insert (Price, Element);
                     else
                        Agent.Log_Trade
                          ("updating bid: "
                           & Concorde.Quantities.Image
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
                     Price   : constant Price_Type :=
                                 Info.Asks.First_Key;
                     Element : constant Offer_Info :=
                                 Info.Asks.First_Element;
                  begin
                     Info.Asks.Delete_First;
                     if Element.Agent.Reference /= Agent.Reference then
                        New_Queue.Insert (Price, Element);
                     else
                        Agent.Log_Trade
                          ("updating ask: "
                           & Concorde.Quantities.Image
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
