with Concorde.Logging;

package body Concorde.Markets is

   function Taxable_Image
     (Price_With_Tax : Concorde.Money.Price_Type;
      Tax_Rate       : Non_Negative_Real)
      return String
     with Unreferenced;

   ---------------------
   -- Check_Commodity --
   ---------------------

   procedure Check_Commodity
     (Market    : Root_Market_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
   is
   begin
      if Market.Commodities.Element (Commodity) = null then
         declare
            New_Cached : constant Cached_Commodity :=
                           new Cached_Commodity_Record'
                             (others => <>);
         begin
            Market.Commodities.Replace_Element
              (Commodity, New_Cached);
         end;
      end if;
   end Check_Commodity;

   -------------------
   -- Create_Market --
   -------------------

   function Create_Market
     (Owner  : not null access constant
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
      Agent     : not null access constant
        Concorde.Trades.Trader_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type)
   is
      use Concorde.Money;
      use Concorde.Quantities;
      Info : constant Cached_Commodity :=
               Market.Get_Commodity (Commodity);
      Remaining : Quantity_Type := Quantity;
      Hist      : Historical_Quantity_Record :=
                    (Concorde.Dates.Current_Date, others => <>);
   begin
      if Info.Current_Price = Zero then
         Info.Current_Price := Price;
         Info.Historical_Mean_Price := Price;
      end if;

      case Offer is
         when Concorde.Trades.Bid =>
            Hist.Quantities (Concorde.Trades.Local_Demand) := Quantity;
            while Remaining > Zero
              and then not Info.Asks.Is_Empty
              and then Price >= Info.Asks.Maximum_Key
            loop
               declare
                  Ask_Info   : Offer_Info := Info.Asks.Maximum_Element;
                  Traded     : constant Quantity_Type :=
                                 Min (Remaining, Ask_Info.Remaining_Quantity);
                  Final_Price  : constant Price_Type :=
                                   Adjust_Price
                                     (Ask_Info.Offer_Price + Price, 0.5);
               begin
                  Info.Asks.Delete_Maximum;
                  Ask_Info.Remaining_Quantity :=
                    Ask_Info.Remaining_Quantity - Traded;
                  Remaining := Remaining - Traded;
                  Hist.Quantities (Concorde.Trades.Total_Traded) :=
                    Hist.Quantities (Concorde.Trades.Total_Traded)
                    + Traded;
                  Info.Current_Supply := Info.Current_Supply - Traded;

                  Market.Execute_Trade
                    (Buyer     => Agent,
                     Seller    => Ask_Info.Agent,
                     Commodity => Commodity,
                     Quantity  => Traded,
                     Price     => Final_Price);

                  Info.Historical_Mean_Price :=
                    Adjust_Price
                      (Info.Historical_Mean_Price + Final_Price, 0.5);

                  if Ask_Info.Remaining_Quantity > Zero then
                     Info.Asks.Insert (Ask_Info.Offer_Price, Ask_Info);
                  end if;
               end;
            end loop;

            if Remaining > Zero then
               Info.Bids.Insert
                 (Price, Offer_Info'(Agent, Quantity, Remaining, Price));
               Info.Current_Demand := Info.Current_Demand + Remaining;
            end if;

         when Concorde.Trades.Ask =>

            Hist.Quantities (Concorde.Trades.Local_Supply) := Quantity;

            while Remaining > Zero
              and then not Info.Bids.Is_Empty
              and then Price <= Info.Bids.Maximum_Key
            loop
               declare
                  Bid_Info     : Offer_Info := Info.Bids.Maximum_Element;
                  Traded       : constant Quantity_Type :=
                                   Min (Remaining,
                                        Bid_Info.Remaining_Quantity);
                  Final_Price  : constant Price_Type :=
                                   Adjust_Price
                                     (Bid_Info.Offer_Price + Price, 0.5);
               begin
                  Info.Bids.Delete_Maximum;
                  Bid_Info.Remaining_Quantity :=
                    Bid_Info.Remaining_Quantity - Traded;
                  Remaining := Remaining - Traded;
                  Hist.Quantities (Concorde.Trades.Total_Traded) :=
                    Hist.Quantities (Concorde.Trades.Total_Traded)
                    + Traded;
                  Info.Current_Demand := Info.Current_Demand - Traded;

                  Market.Execute_Trade
                    (Buyer     => Bid_Info.Agent,
                     Seller    => Agent,
                     Commodity => Commodity,
                     Quantity  => Traded,
                     Price     => Final_Price);

                  Info.Historical_Mean_Price :=
                    Adjust_Price
                      (Info.Historical_Mean_Price + Final_Price, 0.5);

                  if Bid_Info.Remaining_Quantity > Zero then
                     Info.Bids.Insert (Bid_Info.Offer_Price, Bid_Info);
                  end if;
               end;
            end loop;

            if Remaining > Zero then
               Info.Asks.Insert
                 (Price, Offer_Info'(Agent, Quantity, Remaining, Price));
               Info.Current_Supply := Info.Current_Supply + Remaining;
            end if;

      end case;

      declare
         L : Quantity_Metric_Lists.List renames
               Market.Get_Commodity (Commodity).Metrics;
      begin
         L.Insert (L.First, Hist);
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
      use Concorde.Money;
   begin
      return Price : Concorde.Money.Price_Type :=
        Market.Get_Commodity (Commodity).Current_Price
      do
         if Price = Zero then
            Price := Commodity.Base_Price;
         end if;
      end return;
   end Current_Price;

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
--              & Concorde.Dates.Current_Date_To_String
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
      pragma Unreferenced (Market);
      use Concorde.Commodities;
      use Concorde.Money;
   begin
      Buyer.Execute_Trade
        (Offer     => Concorde.Trades.Bid,
         Commodity => Commodity,
         Quantity  => Quantity,
         Cost      => Total (Price, Quantity));

      Seller.Execute_Trade
        (Offer     => Concorde.Trades.Ask,
         Commodity => Commodity,
         Quantity  => Quantity,
         Cost      => Total (Price, Quantity));

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
      Commodity : Commodities.Commodity_Type)
      return Cached_Commodity
   is
   begin
      Market.Check_Commodity (Commodity);
      return Market.Commodities.Element (Commodity);
   end Get_Commodity;

   ------------------
   -- Get_Quantity --
   ------------------

   overriding function Get_Quantity
     (Market    : Root_Market_Type;
      Commodity : Concorde.Commodities.Commodity_Type;
      Metric    : Concorde.Trades.Trade_Metric;
      Start     : Concorde.Dates.Date_Type;
      Finish    : Concorde.Dates.Date_Type)
      return Concorde.Quantities.Quantity_Type
   is
      use Concorde.Dates;
      use Concorde.Quantities;
      use all type Concorde.Trades.Trade_Metric;
      Result : Quantity_Type := Zero;
      Cached : constant Cached_Commodity :=
                 Market.Get_Commodity (Commodity);
   begin
      for Hist of Cached.Metrics loop
         exit when Hist.Date < Start;
         if Hist.Date <= Finish then
            Result := Result + Hist.Quantities (Metric);
         end if;
      end loop;

      if Metric = Local_Demand then
         Result := Result + Cached.Current_Demand;
      elsif Metric = Local_Supply then
         Result := Result + Cached.Current_Supply;
      end if;

      return Result;
   end Get_Quantity;

   ---------------------------
   -- Historical_Mean_Price --
   ---------------------------

   overriding function Historical_Mean_Price
     (Market    : Root_Market_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type
   is
      use Concorde.Money;
   begin
      return Price : Concorde.Money.Price_Type :=
        Market.Get_Commodity (Commodity).Historical_Mean_Price
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
      Info.Current_Price := Price;
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

end Concorde.Markets;
