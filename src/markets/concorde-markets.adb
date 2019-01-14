--  with Ada.Exceptions;
--  with Ada.Text_IO;
--
--  with Concorde.Elementary_Functions;
--  with Concorde.Real_Images;

with Concorde.Agents;

with Concorde.Logging;
with Concorde.Logs;

package body Concorde.Markets is

   function Get_Tax_Category
     (Market : Root_Market_Type'Class;
      Agent  : not null access constant
        Concorde.Trades.Trader_Interface'Class;
      Offer  : Concorde.Trades.Offer_Type)
      return Concorde.Trades.Market_Tax_Category;

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
            New_Cached.Transactions.Create
              (Concorde.Commodities.Commodity_Type (Commodity));
            Market.Commodities.Replace_Element
              (Commodity, New_Cached);
         end;
      end if;
   end Check_Commodity;

   -------------------
   -- Create_Market --
   -------------------

   function Create_Market
     (Identifier : String;
      Owner      : not null access constant Market_Interface'Class;
      Manager    : not null access constant
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

   overriding function Create_Offer
     (Market    : Root_Market_Type;
      Offer     : Concorde.Trades.Offer_Type;
      Trader    : not null access constant
        Concorde.Trades.Trader_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type)
      return Concorde.Trades.Offer_Reference
   is
      use Concorde.Money;
      use Concorde.Quantities;
      Agent : constant Concorde.Agents.Agent_Type :=
                Concorde.Agents.Agent_Type (Trader);
      Info : constant Cached_Commodity :=
                Market.Get_Commodity (Commodity);

      Tax_Category : constant Concorde.Trades.Market_Tax_Category :=
                       Market.Get_Tax_Category
                         (Agent, Offer);
      Offer_Name : constant String :=
                     (case Offer is
                         when Concorde.Trades.Ask =>
                            "ask",
                         when Concorde.Trades.Bid =>
                            "bid");
      Cache      : constant Cached_Commodity :=
                     Market.Get_Commodity (Commodity);
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

      declare
         Id : constant Concorde.Trades.Offer_Reference :=
                Concorde.Trades.Create_Reference;
         Ref : constant Concorde.Transactions.Offer_Reference :=
                 Cache.Transactions.Add_Offer
                   (Agent        => Agent,
                    Offer        => Offer,
                    Quantity     => Quantity,
                    Price        => Price,
                    Tax          =>
                      Market.Manager.Tax_Rate (Tax_Category, Commodity),
                    Tax_Category => Tax_Category);
      begin
         Agent.Log
           ("new "
            & (case Offer is
                 when Concorde.Trades.Bid => "bid",
                 when Concorde.Trades.Ask => "ask")
            & ": "
            & Show (Quantity)
            & " "
            & Commodity.Name
            & " @ "
            & Show (Price)
            & " ea"
            & "; id="
            & Concorde.Trades.Image (Id));
         Market.Update.Offers.Insert (Id, (Commodity, Ref));
         return Id;
      end;

   end Create_Offer;

   -------------------
   -- Current_Price --
   -------------------

   overriding function Current_Price
     (Market    : Root_Market_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type
   is
      use type Concorde.Money.Price_Type;
      Info : constant Cached_Commodity :=
               Market.Get_Commodity (Commodity);
      Price : Concorde.Money.Price_Type :=
                Info.Transactions.Daily_Average_Price;
   begin
      if Price = Concorde.Money.Zero then
         Price := Commodity.Base_Price;
      end if;
      return Price;
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
      return Market.Get_Commodity (Item)
        .Transactions.Daily_Metric (Metric);
   end Current_Quantity;

   -----------------------------
   -- Daily_Transaction_Count --
   -----------------------------

   function Daily_Transaction_Count
     (Market    : Root_Market_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Natural
   is
   begin
      return Market.Get_Commodity (Commodity)
        .Transactions.Daily_Transaction_Count;
   end Daily_Transaction_Count;

   ------------------
   -- Delete_Offer --
   ------------------

   overriding procedure Delete_Offer
     (Market    : Root_Market_Type;
      Reference : Concorde.Trades.Offer_Reference)
   is
      Ref   : constant Market_Offer_Reference :=
                Market.Offers.Element (Reference);
      Info  : constant Cached_Commodity :=
                Market.Get_Commodity (Ref.Commodity);

   begin
      Market.Log
        ("delete offer: "
         & Concorde.Trades.Image (Reference));
      Info.Transactions.Delete_Offer (Ref.Reference);
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

   --------------------------
   -- Execute_Transactions --
   --------------------------

   procedure Execute_Transactions
     (Market : Root_Market_Type'Class)
   is
   begin
      for Commodity of Concorde.Commodities.All_Commodities loop
         if Market.Commodities.Element (Commodity) /= null then
            Market.Get_Commodity (Commodity).Transactions.Resolve;
         end if;
      end loop;
   end Execute_Transactions;

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
     (Market : Root_Market_Type'Class;
      Agent  : not null access constant
        Concorde.Trades.Trader_Interface'Class;
      Offer  : Concorde.Trades.Offer_Type)
      return Concorde.Trades.Market_Tax_Category
   is
      pragma Unreferenced (Market);
   begin
      case Offer is
         when Concorde.Trades.Bid =>
            if Agent.Market_Resident then
               return Concorde.Trades.Purchases;
            else
               return Concorde.Trades.Export;
            end if;
         when Concorde.Trades.Ask =>
            if Agent.Market_Resident then
               return Concorde.Trades.Sales;
            else
               return Concorde.Trades.Import;
            end if;
      end case;
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
   is null;

--        use Concorde.Money;
--        use Concorde.Quantities;
--        Info  : constant Cached_Commodity :=
--                  Market.Get_Commodity (Commodity);
--
--        Offer_Name : constant String :=
--                       (case Offer is
--                           when Concorde.Trades.Ask =>
--                              "ask",
--                           when Concorde.Trades.Bid =>
--                              "bid");
--
--        Market_Log_Path  : constant String :=
--                             Market.Identifier
--                             & "/" & Commodity.Identifier
--                             & "/" & Offer_Name;
--     begin
--
--        if Quantity = Zero then
--           Market.Log ("error: quantity is zero");
--        end if;
--
--        pragma Assert (Quantity > Zero);
--
--        Concorde.Logs.Log_Fields
--          (Market_Log_Path,
--           Trader.Short_Name,
--           Market.Identifier,
--           Commodity.Identifier,
--           Image (Quantity),
--           Image (Price));
--
--  --        Add_Commodity_Offer
--  --          (Info     => Info,
--  --           Offer    => Offer,
--  --           Resident => Trader.Market_Resident,
--  --           Quantity => Quantity,
--  --           Price    => Price);
--
--     end Notify_Foreign_Trade;

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
            if Info.Transactions.Has_Asks then
               return Info.Transactions.First_Ask_Price;
            else
               return Concorde.Money.Zero;
            end if;
         when Concorde.Trades.Bid =>
            if Info.Transactions.Has_Bids then
               return Info.Transactions.First_Bid_Price;
            else
               return Concorde.Money.Zero;
            end if;
      end case;
   end Price;

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

   ------------------------
   -- Update_Offer_Price --
   ------------------------

   overriding procedure Update_Offer_Price
     (Market    : Root_Market_Type;
      Reference : Concorde.Trades.Offer_Reference;
      New_Price : Concorde.Money.Price_Type)
   is
      Ref   : constant Market_Offer_Reference :=
                Market.Offers.Element (Reference);
      Info  : constant Cached_Commodity :=
                Market.Get_Commodity (Ref.Commodity);

   begin
      Info.Transactions.Update_Offer_Price (Ref.Reference, New_Price);
   end Update_Offer_Price;

   ---------------------------
   -- Update_Offer_Quantity --
   ---------------------------

   overriding procedure Update_Offer_Quantity
     (Market       : Root_Market_Type;
      Reference    : Concorde.Trades.Offer_Reference;
      New_Quantity : Concorde.Quantities.Quantity_Type)
   is
      Ref   : constant Market_Offer_Reference :=
                Market.Offers.Element (Reference);
      Info  : constant Cached_Commodity :=
                Market.Get_Commodity (Ref.Commodity);

   begin
      Info.Transactions.Update_Offer_Quantity (Ref.Reference, New_Quantity);
   end Update_Offer_Quantity;

end Concorde.Markets;
