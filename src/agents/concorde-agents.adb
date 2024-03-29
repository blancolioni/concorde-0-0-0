with Ada.Exceptions;

with Concorde.Logging;
with Concorde.Logs;
with Concorde.Random;
with Concorde.Real_Images;

package body Concorde.Agents is

   Log_Offers : Boolean := False;
--   Log_Price_Updates : constant Boolean := True;

   Use_Minimum_Prices : constant Boolean := False;

   Next_Ref   : Agent_Reference := 0;

   function Price_Position_In_Range
     (Value, Low, High : Concorde.Money.Price_Type)
      return Unit_Real;

   function Create_Ask_Price
     (Low, High : Concorde.Money.Price_Type)
      return Concorde.Money.Price_Type;
   --  Come up with an asking price between Low and High.

   function Create_Bid_Price
     (Low, High : Concorde.Money.Price_Type)
      return Concorde.Money.Price_Type;
   --  Come up with an bidding price between Low and High.

   --------------
   -- Add_Cash --
   --------------

   procedure Add_Cash
     (Agent  : in out Root_Agent_Type'Class;
      Amount : Concorde.Money.Money_Type)
   is
      use type Concorde.Money.Money_Type;
   begin
      Agent.Set_Cash (Agent.Cash + Amount);
   end Add_Cash;

   ------------------
   -- Add_Contract --
   ------------------

   overriding procedure Add_Contract
     (Agent    : in out Root_Agent_Type;
      Contract : Concorde.Contracts.Contract_Type)
   is
      use type Concorde.Money.Money_Type;
   begin
      pragma Assert (Contract.Total_Cost
                     <= Root_Agent_Type'Class (Agent).Limit_Cash);

      Agent.Offered_Contracts.Append (Contract);
      Agent.Contracted_Quantities.Add_Quantity
        (Contract.Commodity, Contract.Quantity, Contract.Total_Cost);
      Agent.Remove_Cash (Contract.Total_Cost);
      Agent.Reserved_Cash := Agent.Reserved_Cash + Contract.Total_Cost;

      Agent.Log_Trade
        ("new contract: " & Contract.Show);
   end Add_Contract;

   ---------------------
   -- Cancel_Contract --
   ---------------------

   overriding procedure Cancel_Contract
     (Agent    : in out Root_Agent_Type;
      Contract : Concorde.Contracts.Contract_Type)
   is
      Accepted_Position : Current_Contract_Lists.Cursor :=
                            Agent.Accepted_Contracts.Find (Contract);
      Offered_Position  : Current_Contract_Lists.Cursor :=
                            Agent.Offered_Contracts.Find (Contract);
   begin
      pragma Assert (Current_Contract_Lists.Has_Element (Accepted_Position)
                     or else Current_Contract_Lists.Has_Element
                       (Offered_Position));
      if Current_Contract_Lists.Has_Element (Accepted_Position) then
         Agent.Accepted_Contracts.Delete (Accepted_Position);
      end if;
      if Current_Contract_Lists.Has_Element (Offered_Position) then
         declare
            use Concorde.Money;
            Value : constant Concorde.Money.Money_Type :=
                      Current_Contract_Lists.Element (Offered_Position)
                      .Total_Cost;
         begin
            Agent.Add_Cash (Value);
            Agent.Reserved_Cash := Agent.Reserved_Cash - Value;
         end;

         Agent.Offered_Contracts.Delete (Offered_Position);
      end if;

      Agent.Log_Trade
        ("canceled contract: " & Contract.Show);
   end Cancel_Contract;

   ----------
   -- Cash --
   ----------

   function Cash
     (Agent : Root_Agent_Type)
      return Concorde.Money.Money_Type
   is
   begin
      return Agent.Cash;
   end Cash;

   -----------
   -- Check --
   -----------

   procedure Check (Offer : Agent_Offer) is
      use Concorde.Money;
   begin
      if not (Offer.Valid xor Offer.Price = Zero) then
         raise Constraint_Error with
           "agent offer invariant failed: "
           & (if Offer.Valid then "valid" else "not valid")
           & " and price = "
           & Show (Offer.Price);
      end if;
   end Check;

   ------------------
   -- Check_Offers --
   ------------------

   procedure Check_Offers
     (Agent : in out Root_Agent_Type'Class)
   is
      use Concorde.Money;

      procedure Update_Ask
        (Commodity : not null access constant
           Concorde.Commodities.Root_Commodity_Type'Class;
         Offer     : in out Agent_Offer);

      procedure Update_Bid
        (Commodity : not null access constant
           Concorde.Commodities.Root_Commodity_Type'Class;
         Offer     : in out Agent_Offer);

      ----------------
      -- Update_Ask --
      ----------------

      procedure Update_Ask
        (Commodity : not null access constant
           Concorde.Commodities.Root_Commodity_Type'Class;
         Offer     : in out Agent_Offer)
      is
         use Concorde.Trades;
      begin

         Check (Offer);

         if not Offer.Valid then
            return;
         end if;

         case Agent.Offer_Strategy (Commodity) is
            when Concorde.Trades.Fixed_Price =>
               null;
            when Concorde.Trades.Belief_Based =>
               declare
                  Mean         : constant Price_Type :=
                                   Agent.Market.Historical_Mean_Price
                                     (Commodity);
                  Start_Belief : constant Agent_Price_Belief_Record :=
                                   Agent.Get_Price_Belief
                                     (Agent.Market, Commodity,
                                      Concorde.Trades.Ask);
                  Belief       : Agent_Price_Belief_Record := Start_Belief;
                  Supply       : constant Quantity_Type :=
                                   Agent.Market.Current_Supply (Commodity);
                  Demand       : constant Quantity_Type :=
                                   Agent.Market.Current_Demand (Commodity);
               begin

                  Agent.Log_Trade
                    (Commodity.Name
                     & ": checking offers;"
                     & " current price belief "
                     & Show (Belief.Low)
                     & "/"
                     & Show (Belief.High)
                     & "; mean "
                     & Show (Adjust_Price (Belief.Low + Belief.High, 0.5))
                     & "; historical mean "
                     & Show (Mean)
                     & "; supply/demand "
                     & Show (Supply)
                     & "/"
                     & Show (Demand)
                     & ": offered " & Image (Offer.Quantity)
                     & "; sold " & Image (Offer.Filled)
                     & " at "
                     & Show (Offer.Price) & " with tax");

                  if Offer.Filled > Scale (Offer.Quantity, 0.6) then
                     if Offer.Price < Mean then
                        Translate (Belief, Mean, 0.5);
                     end if;
                     Contract (Belief, 0.05);
                  elsif Offer.Price < Belief.High then
                     if Offer.Price > Mean then
                        Translate (Belief, Mean, 0.5);
                     end if;
                     Expand (Belief, 0.05);
                  end if;

                  Agent.Log_Trade
                    (Commodity.Name
                     & ": new price belief "
                     & Show (Belief.Low)
                     & "/"
                     & Show (Belief.High)
                     & "; mean "
                     & Show (Adjust_Price (Belief.Low + Belief.High, 0.5)));

                  declare
                     Log_Path : constant String :=
                                  "agents/"
                                  & Agent.Class_Name
                                  & "/" & Agent.Short_Name
                                  & "/" & Commodity.Identifier
                                  & "/update-ask";
                  begin
                     Concorde.Logs.Log_Fields
                       (Log_Path,
                        Image (Start_Belief.Low),
                        Image (Start_Belief.High),
                        Image
                          (Adjust_Price
                               (Start_Belief.Low
                                + Start_Belief.High, 0.5)),
                        Image (Mean),
                        Image (Supply),
                        Image (Demand),
                        Image (Offer.Quantity),
                        Image (Offer.Filled),
                        Image (Offer.Price),
                        Image (Belief.Low),
                        Image (Belief.High),
                        Image
                          (Adjust_Price
                               (Belief.Low + Belief.High, 0.5)));

                  end;

                  Agent.Update_Price_Belief (Commodity, Belief);
                  Agent.Market.Delete_Offer
                    (Concorde.Trades.Ask, Agent, Commodity);
                  Offer := (others => <>);
               end;
            when Concorde.Trades.Average_Price =>
               if Offer.Filled < Offer.Quantity then
                  declare
                     Maximum_Price : constant Price_Type :=
                                       Agent.Market.Price
                                         (Concorde.Trades.Ask,
                                          Commodity,
                                          Offer.Quantity - Offer.Filled);
                     Supply        : constant Quantity_Type :=
                                       Agent.Market.Current_Supply (Commodity);
                     Demand        : constant Quantity_Type :=
                                       Agent.Market.Current_Demand (Commodity);
                  begin

                     Agent.Log_Trade
                       (Commodity.Name
                        & ": checking offers"
                        & "; max price "
                        & Show (Maximum_Price)
                        & "; supply/demand "
                        & Show (Supply)
                        & "/"
                        & Show (Demand)
                        & ": offered " & Show (Offer.Quantity)
                        & "; sold " & Show (Offer.Filled)
                        & " at "
                        & Show (Offer.Price));

                     if Maximum_Price /= Zero then
                        Offer.Price := Adjust_Price (Maximum_Price, 0.9);
                        Agent.Market.Update_Offer
                          (Offer     => Concorde.Trades.Ask,
                           Trader    => Agent,
                           Commodity => Commodity,
                           New_Price => Offer.Price);
                     else
                        Offer.Price :=
                          Adjust_Price
                            (Offer.Price,
                             Concorde.Random.Unit_Random * 0.1 + 0.9);
                        Agent.Market.Update_Offer
                          (Offer     => Concorde.Trades.Ask,
                           Trader    => Agent,
                           Commodity => Commodity,
                           New_Price => Offer.Price);
                     end if;
                  end;
               end if;
         end case;
      end Update_Ask;

      ----------------
      -- Update_Bid --
      ----------------

      procedure Update_Bid
        (Commodity : not null access constant
           Concorde.Commodities.Root_Commodity_Type'Class;
         Offer     : in out Agent_Offer)
      is
      begin

         Check (Offer);

         if not Offer.Valid then
            return;
         end if;

         case Agent.Offer_Strategy (Commodity) is
            when Concorde.Trades.Fixed_Price =>
               null;
            when Concorde.Trades.Belief_Based =>
               declare
                  Mean         : constant Price_Type :=
                                   Agent.Market.Historical_Mean_Price
                                     (Commodity);
                  Start_Belief : constant Agent_Price_Belief_Record :=
                                   Agent.Get_Price_Belief
                                     (Agent.Market, Commodity,
                                      Concorde.Trades.Bid);
                  Belief       : Agent_Price_Belief_Record := Start_Belief;
                  Supply       : constant Quantity_Type :=
                                   Agent.Market.Current_Supply (Commodity);
                  Demand       : constant Quantity_Type :=
                                   Agent.Market.Current_Demand (Commodity);
               begin

                  Agent.Log_Trade
                    (Commodity.Name
                     & ": checking offers;"
                     & " current price belief "
                     & Show (Belief.Low)
                     & "/"
                     & Show (Belief.High)
                     & "; mean "
                     & Show (Adjust_Price (Belief.Low + Belief.High, 0.5))
                     & "; historical mean "
                     & Show (Mean)
                     & "; supply/demand "
                     & Show (Supply)
                     & "/"
                     & Show (Demand)
                     & ": wanted " & Image (Offer.Quantity)
                     & "; got " & Image (Offer.Filled)
                     & " at "
                     & Show (Offer.Price));

                  if Supply < Scale (Demand, 0.2) then
                     --  little supply, so don't move our price
                     null;
                  elsif Offer.Filled = Offer.Quantity then
                     if Offer.Price > Mean then
                        Translate (Belief, Mean, 0.5);
                     end if;
                     Contract (Belief, 0.05);
                  else
                     if Offer.Price < Mean then
                        Translate (Belief, Mean, 0.5);
                     end if;
                     Expand (Belief, 0.05);
                  end if;

                  Agent.Log_Trade
                    (Commodity.Name
                     & ": new price belief "
                     & Show (Belief.Low)
                     & "/"
                     & Show (Belief.High)
                     & "; mean "
                     & Show (Adjust_Price (Belief.Low + Belief.High, 0.5)));

                  declare
                     Log_Path : constant String :=
                                  "agents/"
                                  & Agent.Class_Name
                                  & "/" & Agent.Short_Name
                                  & "/" & Commodity.Identifier
                                  & "/update-bid";
                  begin
                     Concorde.Logs.Log_Fields
                       (Log_Path,
                        Image (Start_Belief.Low),
                        Image (Start_Belief.High),
                        Image
                          (Adjust_Price
                               (Start_Belief.Low
                                + Start_Belief.High, 0.5)),
                        Image (Mean),
                        Image (Supply),
                        Image (Demand),
                        Image (Offer.Quantity),
                        Image (Offer.Filled),
                        Image (Offer.Price),
                        Image (Belief.Low),
                        Image (Belief.High),
                        Image
                          (Adjust_Price
                               (Belief.Low + Belief.High, 0.5)));
                  end;

                  Agent.Update_Price_Belief (Commodity, Belief);
                  Agent.Market.Delete_Offer
                    (Concorde.Trades.Bid, Agent, Commodity);

                  Offer := (others => <>);
               end;
            when Concorde.Trades.Average_Price =>
               if Offer.Filled < Offer.Quantity then
                  declare
                     Minimum_Price : constant Price_Type :=
                                       Agent.Market.Price
                                         (Concorde.Trades.Bid,
                                          Commodity,
                                          Offer.Quantity - Offer.Filled);
                     Supply        : constant Quantity_Type :=
                                       Agent.Market.Current_Supply (Commodity);
                     Demand        : constant Quantity_Type :=
                                       Agent.Market.Current_Demand (Commodity);
                  begin

                     Agent.Log_Trade
                       (Commodity.Name
                        & ": checking offers"
                        & "; min price "
                        & Show (Minimum_Price)
                        & "; supply/demand "
                        & Show (Supply)
                        & "/"
                        & Show (Demand)
                        & ": wanted " & Image (Offer.Quantity)
                        & "; got " & Image (Offer.Filled)
                        & " at "
                        & Show (Offer.Price));

                     if Supply = Zero
                       or else Total (Minimum_Price, Unit) > Agent.Limit_Cash
                       or else Total (Offer.Price, Unit) > Agent.Limit_Cash

                     then
                        --  this ain't gonna work
                        Agent.Market.Delete_Offer
                          (Offer     => Concorde.Trades.Bid,
                           Trader    => Agent,
                           Commodity => Commodity);
                        declare
                           use Concorde.Commodities;
                           Cancel : Current_Contract_Lists.List;
                        begin
                           for Contract of
                             Agent.Accepted_Contracts
                           loop
                              if Contract.Commodity = Commodity then
                                 Cancel.Append (Contract);
                              end if;
                           end loop;

                           for Item of Cancel loop
                              Agent.Cancel_Contract (Item);
                           end loop;
                        end;

                        Offer := (others => <>);
                     elsif Minimum_Price /= Zero then
                        Offer.Price := Adjust_Price (Minimum_Price, 1.1);
                        Agent.Market.Update_Offer
                          (Offer     => Concorde.Trades.Bid,
                           Trader    => Agent,
                           Commodity => Commodity,
                           New_Price => Offer.Price);
                     end if;
                  end;
               end if;
         end case;
      end Update_Bid;

   begin
      Agent.Age := Agent.Age + 1;
      Agent.Bids.Update (Update_Bid'Access);
      Agent.Asks.Update (Update_Ask'Access);
   end Check_Offers;

   ---------------------------
   -- Clear_Current_Account --
   ---------------------------

   procedure Clear_Current_Account
     (Agent : in out Root_Agent_Type'Class)
   is
   begin
      Agent.Last_Earnings := Concorde.Money.Zero;
      Agent.Last_Expenses := Concorde.Money.Zero;
   end Clear_Current_Account;

   -----------------------
   -- Clear_Filled_Asks --
   -----------------------

   procedure Clear_Filled_Asks
     (Agent     : in out Root_Agent_Type'Class)
   is
      procedure Clear
        (Item  : not null access constant
           Concorde.Commodities.Root_Commodity_Type'Class;
         Offer : in out Agent_Offer);

      -----------
      -- Clear --
      -----------

      procedure Clear
        (Item  : not null access constant
           Concorde.Commodities.Root_Commodity_Type'Class;
         Offer : in out Agent_Offer)
      is
         pragma Unreferenced (Item);
      begin
         Check (Offer);
         if Offer.Valid and then Offer.Filled = Offer.Quantity then
            Offer := (others => <>);
            Check (Offer);
         end if;
      end Clear;

   begin
      Agent.Asks.Update (Clear'Access);
   end Clear_Filled_Asks;

   -----------------------
   -- Clear_Filled_Bids --
   -----------------------

   procedure Clear_Filled_Bids
     (Agent     : in out Root_Agent_Type'Class)
   is
      procedure Clear
        (Item  : not null access constant
           Concorde.Commodities.Root_Commodity_Type'Class;
         Offer : in out Agent_Offer);

      -----------
      -- Clear --
      -----------

      procedure Clear
        (Item  : not null access constant
           Concorde.Commodities.Root_Commodity_Type'Class;
         Offer : in out Agent_Offer)
      is
         pragma Unreferenced (Item);
      begin
         Check (Offer);
         if Offer.Valid and then Offer.Filled = Offer.Quantity then
            Offer := (others => <>);
            Check (Offer);
         end if;
      end Clear;

   begin
      Agent.Bids.Update (Clear'Access);
   end Clear_Filled_Bids;

   -----------------
   -- Clear_Stock --
   -----------------

   overriding procedure Clear_Stock
     (Agent    : in out Root_Agent_Type)
   is
   begin
      Agent.Stock.Clear_Stock;
   end Clear_Stock;

   -------------------------------
   -- Close_Completed_Contracts --
   -------------------------------

   overriding procedure Close_Completed_Contracts
     (Agent   : in out Root_Agent_Type)
   is
      Changed  : Boolean := False;
      New_List : Current_Contract_Lists.List;
   begin
      for Contract of Agent.Offered_Contracts loop
         if Contract.Completed then
            Agent.Log_Trade ("closing: " & Contract.Show);
            declare
               use Current_Contract_Lists;
               Accepted : Cursor := Agent.Accepted_Contracts.Find (Contract);
            begin
               if Has_Element (Accepted) then
                  Agent.Accepted_Contracts.Delete (Accepted);
               end if;
            end;
            Agent.Contracted_Quantities.Remove_Quantity
              (Contract.Commodity, Contract.Quantity, Contract.Total_Cost);
            Changed := True;
         else
            New_List.Append (Contract);
         end if;
      end loop;
      if Changed then
         Agent.Offered_Contracts := New_List;
      end if;
   end Close_Completed_Contracts;

   --------------
   -- Contract --
   --------------

   procedure Contract
     (Belief : in out Agent_Price_Belief_Record;
      Factor : Unit_Real)
   is
      use Concorde.Money;
   begin
      Belief.Low := Belief.Low
        + Adjust_Price (Belief.High - Belief.Low, Factor / 2.0);
      Belief.High := Belief.High
        - Adjust_Price (Belief.High - Belief.Low, Factor / 2.0);
   end Contract;

   ----------------
   -- Create_Ask --
   ----------------

   procedure Create_Ask
     (Agent        : not null access constant Root_Agent_Type'Class;
      Commodity    : Concorde.Commodities.Commodity_Type;
      Ask_Quantity : Concorde.Quantities.Quantity_Type)
   is
   begin
      Agent.Create_Ask (Commodity, Ask_Quantity,
                        Agent.Create_Ask_Price (Commodity));
   end Create_Ask;

   ----------------
   -- Create_Ask --
   ----------------

   procedure Create_Ask
     (Agent        : not null access constant Root_Agent_Type'Class;
      Commodity    : Concorde.Commodities.Commodity_Type;
      Ask_Quantity : Concorde.Quantities.Quantity_Type;
      Ask_Price    : Concorde.Money.Price_Type)
   is
      use Concorde.Money;
      use Concorde.Trades;

      Market : constant access constant
        Concorde.Trades.Trade_Interface'Class :=
          Agent.Market;

      Mean          : constant Price_Type :=
                        Market.Historical_Mean_Price
                          (Commodity);
      Belief        : constant Agent_Price_Belief_Record :=
                        Agent.Get_Price_Belief (Market, Commodity,
                                                Concorde.Trades.Ask);
      Minimum_Price : constant Price_Type :=
                        Agent.Minimum_Ask_Price (Commodity);

      Sell_Price : constant Price_Type :=
                     (if not Use_Minimum_Prices
                      then (if Agent.Offer_Strategy (Commodity)
                        = Average_Price
                        then Mean
                        else Create_Ask_Price (Belief.Low, Belief.High))
                      else (if Agent.Offer_Strategy (Commodity)
                        = Average_Price
                        then Max (Mean, Minimum_Price)
                        else Create_Ask_Price
                          (Max (Minimum_Price, Belief.Low),
                           Max (Minimum_Price, Belief.High))));
      Sell_Quantity : constant Quantity_Type := Ask_Quantity;

      procedure Update_Agent
        (A : not null access Root_Agent_Type'Class);

      procedure Update_Offer
        (Offer : in out Agent_Offer);

      ------------------
      -- Update_Agent --
      ------------------

      procedure Update_Agent
        (A : not null access Root_Agent_Type'Class)
      is
      begin
         A.Asks.Update_Element (Commodity, Update_Offer'Access);
      end Update_Agent;

      ------------------
      -- Update_Offer --
      ------------------

      procedure Update_Offer
        (Offer : in out Agent_Offer)
      is
      begin

         Check (Offer);

         if Offer.Valid then
            Offer.Quantity := Offer.Quantity + Sell_Quantity;
            Offer.Price := Sell_Price;
            Agent.Log_Trade
              (Commodity.Name
               & ": updated ask "
               & Show (Offer.Quantity) & "/" & Show (Offer.Price));
         else
            Offer := (True, Sell_Price, Sell_Quantity, Zero);
            Agent.Log_Trade
              (Commodity.Name
               & ": new ask "
               & Show (Offer.Quantity) & "/" & Show (Offer.Price));
         end if;
      end Update_Offer;

   begin
      declare
         Log_Path : constant String :=
                      "agents/"
                      & Agent.Class_Name
                      & "/" & Agent.Short_Name
                      & "/" & Commodity.Identifier
                      & "/ask";
      begin
         Concorde.Logs.Log_Fields
           (Log_Path,
            Image (Belief.Low),
            Image (Belief.High),
            Image (Mean),
            Image (Ask_Quantity),
            Image (Minimum_Price),
            Image (Agent.Cash),
            Image (Sell_Price),
            Image (Sell_Quantity));
      end;

      if Sell_Quantity > Zero then
         Update_Agent (Agent.Variable_Reference);
         Market.Create_Offer
           (Offer     => Concorde.Trades.Ask,
            Trader    => Agent,
            Commodity => Commodity,
            Quantity  => Ask_Quantity,
            Price     => Ask_Price);

      end if;

   exception
      when E : others =>
         Agent.Log_Trade
           ("while creating ask for " & Commodity.Name
            & ": "
            & Ada.Exceptions.Exception_Message (E));
   end Create_Ask;

   ----------------------
   -- Create_Ask_Price --
   ----------------------

   function Create_Ask_Price
     (Agent        : not null access constant Root_Agent_Type'Class;
      Commodity    : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type
   is
      use Concorde.Money;
      use Concorde.Trades;

      Market                                : constant access constant
        Concorde.Trades.Trade_Interface'Class :=
          Agent.Market;

      Mean          : constant Price_Type :=
                        Market.Current_Price
                          (Commodity);
      Belief        : constant Agent_Price_Belief_Record :=
                        Agent.Get_Price_Belief (Market, Commodity,
                                                Concorde.Trades.Ask);
      Minimum_Price : constant Price_Type :=
                        (if Use_Minimum_Prices
                         then Agent.Minimum_Ask_Price (Commodity)
                         else Belief.Low);
      Favourability   : constant Unit_Real :=
                          (if Belief.Low = Belief.High
                           then 1.0
                           else Price_Position_In_Range
                             (Mean, Belief.Low, Belief.High));
      Sell_Price : constant Price_Type :=
                          (if Agent.Offer_Strategy (Commodity)
                           = Average_Price
                           then Max (Mean, Minimum_Price)
                           else Create_Ask_Price
                             (Max (Minimum_Price, Belief.Low),
                              Max (Minimum_Price, Belief.High)));
   begin
      if Log_Offers then
         Agent.Log_Trade
           (Commodity.Name
            & ": price belief "
            & Show (Belief.Low)
            & "/"
            & Show (Belief.High)
            & "; mean "
            & Show (Mean)
            & "; minimum "
            & Show (Minimum_Price)
            & "; favourability "
            & Concorde.Real_Images.Approximate_Image (Favourability)
            & "; cash "
            & Show (Agent.Cash)
            & "; sell price "
            & Show (Sell_Price));
      end if;

      return Sell_Price;
   end Create_Ask_Price;

   ----------------------
   -- Create_Ask_Price --
   ----------------------

   function Create_Ask_Price
     (Low, High : Concorde.Money.Price_Type)
      return Concorde.Money.Price_Type
   is
      use Concorde.Money;
      Factor : constant Unit_Real :=
                 Concorde.Random.Unit_Random;
   begin
      return Adjust_Price (High - Low, Factor) + Low;
   end Create_Ask_Price;

   ----------------
   -- Create_Bid --
   ----------------

   procedure Create_Bid
     (Agent        : not null access constant Root_Agent_Type'Class;
      Commodity    : Concorde.Commodities.Commodity_Type;
      Bid_Quantity : Concorde.Quantities.Quantity_Type)
   is
   begin
      Agent.Create_Bid (Commodity, Bid_Quantity,
                        Agent.Create_Bid_Price (Commodity));
   end Create_Bid;

   ----------------
   -- Create_Bid --
   ----------------

   procedure Create_Bid
     (Agent        : not null access constant Root_Agent_Type'Class;
      Commodity    : Concorde.Commodities.Commodity_Type;
      Bid_Quantity : Concorde.Quantities.Quantity_Type;
      Bid_Price    : Concorde.Money.Price_Type)
   is
      use Concorde.Money;
      use Concorde.Trades;

      Market : constant access constant
        Concorde.Trades.Trade_Interface'Class :=
          Agent.Market;

      Mean          : constant Price_Type :=
                        Market.Historical_Mean_Price
                          (Commodity);
      Belief        : constant Agent_Price_Belief_Record :=
                        Agent.Get_Price_Belief (Market, Commodity,
                                                Concorde.Trades.Bid);
      Buy_Price     : constant Price_Type :=
                        (if Agent.Offer_Strategy (Commodity)
                         = Average_Price
                         then Adjust_Price (Mean, 1.1)
                         else Create_Bid_Price
                           (Belief.Low, Belief.High));
      procedure Update_Agent
        (A : not null access Root_Agent_Type'Class);

      procedure Update_Offer
        (Offer : in out Agent_Offer);

      ------------------
      -- Update_Agent --
      ------------------

      procedure Update_Agent
        (A : not null access Root_Agent_Type'Class)
      is
      begin
         A.Bids.Update_Element (Commodity, Update_Offer'Access);
      end Update_Agent;

      ------------------
      -- Update_Offer --
      ------------------

      procedure Update_Offer
        (Offer : in out Agent_Offer)
      is
      begin

         Check (Offer);

         if Offer.Valid then
            Offer.Quantity := Offer.Quantity + Bid_Quantity;
            Offer.Price := Buy_Price;
            Agent.Log_Trade
              (Commodity.Name
               & ": updated bid "
               & Image (Offer.Quantity) & "/" & Image (Offer.Price));
         else
            Offer := (True, Buy_Price, Bid_Quantity, Zero);
            Agent.Log_Trade
              (Commodity.Name
               & ": new bid "
               & Image (Offer.Quantity) & "/" & Image (Offer.Price));
         end if;
      end Update_Offer;

   begin

      declare
         Log_Path : constant String :=
                      "agents/"
                      & Agent.Class_Name
                      & "/" & Agent.Short_Name
                      & "/" & Commodity.Identifier
                      & "/bid";
      begin
         Concorde.Logs.Log_Fields
           (Log_Path,
            Image (Belief.Low),
            Image (Belief.High),
            Image (Mean),
            Image (Bid_Quantity),
            Image (Agent.Cash),
            Image (Buy_Price),
            Image (Bid_Quantity));
      end;

      if Bid_Quantity > Zero then
         Update_Agent (Agent.Variable_Reference);
         Market.Create_Offer
           (Offer     => Concorde.Trades.Bid,
            Trader    => Agent,
            Commodity => Commodity,
            Quantity  => Bid_Quantity,
            Price     => Bid_Price);
      end if;

   exception

      when E : others =>
         Agent.Log_Trade
           ("while creating bid for " & Commodity.Name
            & ": "
            & Ada.Exceptions.Exception_Message (E));
   end Create_Bid;

   ----------------------
   -- Create_Bid_Price --
   ----------------------

   function Create_Bid_Price
     (Agent        : not null access constant Root_Agent_Type'Class;
      Commodity    : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type
   is
      use Concorde.Money;
      use Concorde.Trades;

      Market                                : constant access constant
        Concorde.Trades.Trade_Interface'Class :=
          Agent.Market;

      Mean          : constant Price_Type :=
                        Market.Current_Price
                          (Commodity);
      Belief        : constant Agent_Price_Belief_Record :=
                        Agent.Get_Price_Belief (Market, Commodity,
                                                Concorde.Trades.Bid);
      Favourability : constant Unit_Real :=
                        (if Belief.Low = Belief.High
                         then 1.0
                         else 1.0 - Price_Position_In_Range
                           (Mean, Belief.Low, Belief.High));
      Buy_Price     : constant Price_Type :=
                        (if Agent.Offer_Strategy (Commodity)
                         = Average_Price
                         then Adjust_Price (Mean, 1.1)
                         else Create_Bid_Price
                           (Belief.Low, Belief.High));
   begin
      if Log_Offers then
         Agent.Log_Trade
           (Commodity.Name
            & ": price belief "
            & Show (Belief.Low)
            & "/"
            & Show (Belief.High)
            & "; mean "
            & Show (Mean)
            & "; favourability "
            & Concorde.Real_Images.Approximate_Image (Favourability)
            & "; buy price "
            & Show (Buy_Price));
      end if;
      return Buy_Price;
   end Create_Bid_Price;

   ----------------------
   -- Create_Bid_Price --
   ----------------------

   function Create_Bid_Price
     (Low, High : Concorde.Money.Price_Type)
      return Concorde.Money.Price_Type
   is
      use Concorde.Money;
      Factor : constant Unit_Real :=
                 Concorde.Random.Unit_Random;
   begin
      return Adjust_Price (High - Low, Factor) + Low;
   end Create_Bid_Price;

   --------------------------
   -- Current_Ask_Quantity --
   --------------------------

   function Current_Ask_Quantity
     (Agent     : Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
   is
      Info : constant Agent_Offer := Agent.Asks.Element (Commodity);
   begin

      Check (Info);

      return (if Info.Valid then Info.Quantity - Info.Filled
              else Concorde.Quantities.Zero);
   end Current_Ask_Quantity;

   --------------------------
   -- Current_Bid_Quantity --
   --------------------------

   function Current_Bid_Quantity
     (Agent     : Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
   is
      Info : constant Agent_Offer := Agent.Bids.Element (Commodity);
   begin

      Check (Info);

      return (if Info.Valid then Info.Quantity - Info.Filled
              else Concorde.Quantities.Zero);
   end Current_Bid_Quantity;

   ----------------------
   -- Current_Location --
   ----------------------

   overriding function Current_Location
     (Agent : Root_Agent_Type)
      return Concorde.Locations.Object_Location
   is
   begin
      return Agent.Location;
   end Current_Location;

   ------------------
   -- Daily_Budget --
   ------------------

   function Daily_Budget
     (Agent     : Root_Agent_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Money_Type
   is
      pragma Unreferenced (Agent, Commodity);
   begin
      return Concorde.Money.Zero;
   end Daily_Budget;

   ------------------
   -- Daily_Desire --
   ------------------

   function Daily_Desire
     (Agent     : Root_Agent_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Root_Agent_Type'Class (Agent).Daily_Needs (Commodity);
   end Daily_Desire;

   -----------------
   -- Daily_Needs --
   -----------------

   function Daily_Needs
     (Agent     : Root_Agent_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
   is
      pragma Unreferenced (Agent, Commodity);
   begin
      return Concorde.Quantities.Zero;
   end Daily_Needs;

   ------------------
   -- Daily_Supply --
   ------------------

   function Daily_Supply
     (Agent     : Root_Agent_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
   is
      pragma Unreferenced (Agent, Commodity);
   begin
      return Concorde.Quantities.Zero;
   end Daily_Supply;

   ---------------------------
   -- Delete_Pending_Offers --
   ---------------------------

   procedure Delete_Pending_Offers
     (Agent     : in out Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
   is
      use Concorde.Commodities;
      Delete_List : Current_Contract_Lists.List;
   begin
      for Contract of Agent.Offered_Contracts loop
         if Contract.Commodity = Commodity
           and then not Current_Contract_Lists.Has_Element
             (Agent.Accepted_Contracts.Find (Contract))
         then
            Delete_List.Append (Contract);
         end if;
      end loop;

      for Contract of Delete_List loop
         Agent.Cancel_Contract (Contract);
      end loop;
   end Delete_Pending_Offers;

   --------------------------
   -- Enable_Offer_Logging --
   --------------------------

   procedure Enable_Offer_Logging (Enabled : Boolean := True) is
   begin
      Log_Offers := Enabled;
   end Enable_Offer_Logging;

   -------------------
   -- Execute_Trade --
   -------------------

   overriding procedure Execute_Trade
     (Agent     : not null access constant Root_Agent_Type;
      Offer     : Concorde.Trades.Offer_Type;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Cost      : Concorde.Money.Money_Type)
   is

      procedure Execute_Ask
        (A : not null access Root_Agent_Type'Class);

      procedure Execute_Bid
        (A : not null access Root_Agent_Type'Class);

      procedure Update_Offer
        (Offer : in out Agent_Offer);

      -----------------
      -- Execute_Ask --
      -----------------

      procedure Execute_Ask
        (A : not null access Root_Agent_Type'Class)
      is
      begin
         if Quantity > A.Get_Quantity (Commodity) then
            A.Log ("error: ask was for " & Show (Quantity)
                   & " " & Commodity.Name & " but only have "
                   & Show (A.Get_Quantity (Commodity)));
         else
            A.Remove_Quantity (Commodity, Quantity, Cost);
            A.Add_Cash (Cost);
            A.Asks.Update_Element (Commodity, Update_Offer'Access);
         end if;
      end Execute_Ask;

      -----------------
      -- Execute_Bid --
      -----------------

      procedure Execute_Bid
        (A : not null access Root_Agent_Type'Class)
      is
      begin
         A.Add_Quantity (Commodity, Quantity, Cost);
         A.Remove_Cash (Cost);
         A.Bids.Update_Element (Commodity, Update_Offer'Access);
      end Execute_Bid;

      ------------------
      -- Update_Offer --
      ------------------

      procedure Update_Offer
        (Offer : in out Agent_Offer)
      is
      begin
         Check (Offer);
         Offer.Filled := Offer.Filled + Quantity;
         if Offer.Filled = Offer.Quantity then
            Offer := Agent_Offer'
              (Valid    => False,
               Price    => Concorde.Money.Zero,
               Quantity => Concorde.Quantities.Zero,
               Filled   => Concorde.Quantities.Zero);
         end if;
      end Update_Offer;

   begin

--        pragma Assert (Quantity > Zero);

      declare
         use Concorde.Money;
         use Concorde.Trades;
         Offered_Text : constant String :=
                          (case Offer is
                              when Ask => "sells",
                              when Bid => "buys");
         Trader_Price : constant Price_Type :=
                          Concorde.Money.Price
                            (Cost, Quantity);
      begin
         Agent.Log_Price
           (Commodity.Name
            & ": " & Offered_Text & " "
            & Image (Quantity)
            & " @ "
            & Image (Trader_Price)
            & "; total " & Image (Cost));
      end;

      case Offer is
         when Concorde.Trades.Bid =>
            Execute_Bid
              (Root_Agent_Type'Class (Agent.all).Variable_Reference);
         when Concorde.Trades.Ask =>
            Execute_Ask
              (Root_Agent_Type'Class (Agent.all).Variable_Reference);
      end case;

      Root_Agent_Type'Class (Agent.all).Variable_Reference.Account.Append
        ((Date       => Concorde.Calendar.Clock,
          Item       => Commodity,
          Entry_Type => Offer,
          Quantity   => Quantity,
          Cost       => Cost,
          Balance    => Agent.Cash));

   exception
      when E : others =>
         Agent.Log_Trade
           ("caught " & Ada.Exceptions.Exception_Name (E)
            & " while executing trade: "
            & Ada.Exceptions.Exception_Message (E));
   end Execute_Trade;

   ------------
   -- Expand --
   ------------

   procedure Expand
     (Belief : in out Agent_Price_Belief_Record;
      Factor : Unit_Real)
   is
      use Concorde.Money;
   begin
      if Belief.High - Belief.Low > Belief.Low then
         Belief.Low := Adjust_Price (Belief.Low, 1.0 - Factor);
         Belief.High := Adjust_Price (Belief.High, 1.0 + Factor);
      end if;
   end Expand;

   ----------------------
   -- Get_Agent_Access --
   ----------------------

   function Get_Agent_Access
     (Agent : Root_Agent_Type'Class)
      return Agent_Type
   is
   begin
      return Agent_Type (Agent.Object_Database.Element (Agent.Reference));
   end Get_Agent_Access;

   ----------------------
   -- Get_Price_Belief --
   ----------------------

   function Get_Price_Belief
     (Agent     : Root_Agent_Type'Class;
      Market    : not null access constant
        Concorde.Trades.Trade_Interface'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      For_Offer : Concorde.Trades.Offer_Type)
      return Agent_Price_Belief_Record
   is
      use Concorde.Money;
      use Concorde.Trades;
   begin
      case Agent.Offer_Strategy (Commodity) is
         when Belief_Based =>
            if Agent.Belief.Element (Commodity) = null then
               declare
                  Base        : constant Price_Type :=
                                  Market.Historical_Mean_Price (Commodity);
                  Full        : constant Price_Type := Base;
                  Low_Factor  : Non_Negative_Real;
                  High_Factor : Non_Negative_Real;
               begin
                  case For_Offer is
                     when Ask =>
                        Low_Factor := 0.95;

                        if Agent.Age < 10 then
                           High_Factor :=
                             1.05 - Non_Negative_Real (10 - Agent.Age) / 100.0;
                        else
                           High_Factor := 1.05;
                        end if;
                     when Bid =>
                        High_Factor := 1.05;
                        if Agent.Age < 10 then
                           Low_Factor :=
                             0.95 + Non_Negative_Real (10 - Agent.Age) / 100.0;
                        else
                           Low_Factor := 0.95;
                        end if;
                  end case;

                  return (Low => Adjust_Price (Full, Low_Factor),
                          High => Adjust_Price (Full, High_Factor),
                          Strength => 0.5);
               end;
            else
               return Agent.Belief.Element (Commodity).all;
            end if;
         when Fixed_Price =>
            return (Low => Commodity.Base_Price,
                    High => Commodity.Base_Price,
                    Strength => 1.0);
         when Average_Price =>
            return (Low      => Market.Historical_Mean_Price (Commodity),
                    High     => Market.Historical_Mean_Price (Commodity),
                    Strength => 1.0);
      end case;
   end Get_Price_Belief;

   ------------------
   -- Get_Quantity --
   ------------------

   overriding function Get_Quantity
     (Agent      : Root_Agent_Type;
      Commodity  : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Agent.Stock.Get_Quantity (Commodity);
   end Get_Quantity;

   ---------------
   -- Get_Value --
   ---------------

   overriding function Get_Value
     (Agent : Root_Agent_Type;
      Item  : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Money_Type
   is
   begin
      return Agent.Stock.Get_Value (Item);
   end Get_Value;

   ----------------
   -- Has_Market --
   ----------------

   function Has_Market
     (Agent : Root_Agent_Type'Class)
      return Boolean
   is
   begin
      return Agent.Market /= null;
   end Has_Market;

   ----------------
   -- Is_Virtual --
   ----------------

   overriding function Is_Virtual
     (Agent : Root_Agent_Type)
      return Boolean
   is
      pragma Unreferenced (Agent);
   begin
      return False;
   end Is_Virtual;

   ------------------
   -- Leave_Market --
   ------------------

   procedure Leave_Market
     (Agent  : in out Root_Agent_Type'Class)
   is
   begin
      Agent.Market := null;
   end Leave_Market;

   ----------------
   -- Limit_Cash --
   ----------------

   function Limit_Cash
     (Agent : Root_Agent_Type'Class)
      return Concorde.Money.Money_Type
   is
      use type Concorde.Money.Money_Type;
   begin
      if Agent.Guarantor /= null then
         return Agent.Cash + Agent.Guarantor.Limit_Cash;
      else
         return Agent.Cash;
      end if;
   end Limit_Cash;

   --------------
   -- Location --
   --------------

--     function Location
--       (Agent : Root_Agent_Type'Class)
--        return access constant Agent_Location_Interface'Class
--     is
--     begin
--        return Agent.Location;
--     end Location;

   overriding function Location_At
     (Agent : Root_Agent_Type;
      Time  : Concorde.Calendar.Time)
      return Concorde.Locations.Object_Location
   is
      pragma Unreferenced (Time);
   begin
      return Root_Agent_Type'Class (Agent).Current_Location;
   end Location_At;

   --------------------
   -- Location_Index --
   --------------------

--     function Location_Index
--       (Agent : Root_Agent_Type'Class)
--        return Natural
--     is
--     begin
--        return Agent.Location_Index;
--     end Location_Index;

   ---------
   -- Log --
   ---------

   procedure Log
     (Agent    : Root_Agent_Type'Class;
      Category : String;
      Message  : String)
   is
   begin
      Concorde.Logging.Log
        (Agent.Identifier, Concorde.Locations.Short_Name (Agent.Location),
         Category, Message);
   end Log;

   --------------------
   -- Log_Government --
   --------------------

   procedure Log_Government
     (Agent   : Root_Agent_Type'Class;
      Message : String)
   is
   begin
      Agent.Log ("govern", Message);
   end Log_Government;

   ------------------
   -- Log_Movement --
   ------------------

   procedure Log_Movement
     (Agent   : Root_Agent_Type'Class;
      Message : String)
   is
   begin
      Agent.Log ("move", Message);
   end Log_Movement;

   ---------------
   -- Log_Price --
   ---------------

   procedure Log_Price
     (Agent   : Root_Agent_Type'Class;
      Message : String)
   is
   begin
      Agent.Log ("price", Message);
   end Log_Price;

   --------------------
   -- Log_Production --
   --------------------

   procedure Log_Production
     (Agent   : Root_Agent_Type'Class;
      Message : String)
   is
   begin
      Agent.Log ("production", Message);
   end Log_Production;

   ---------------
   -- Log_Trade --
   ---------------

   procedure Log_Trade
     (Agent   : Root_Agent_Type'Class;
      Message : String)
   is
   begin
      Agent.Log ("trade", Message);
   end Log_Trade;

   ---------------------
   -- Log_Transaction --
   ---------------------

   procedure Log_Transaction
     (Buyer    : Root_Agent_Type'Class;
      Seller   : not null access constant Root_Agent_Type'Class;
      Item     : Concorde.Commodities.Commodity_Type;
      Quantity : Concorde.Quantities.Quantity_Type;
      Price    : Concorde.Money.Price_Type)
   is
   begin
      Buyer.Log_Trade
        ("pay " & Seller.Short_Name
         & " for " & Concorde.Quantities.Image (Quantity)
         & " " & Item.Name
         & " @ " & Concorde.Money.Image (Price)
         & "; total "
         & Concorde.Money.Image (Concorde.Money.Total (Price, Quantity))
         & "; cash now " & Concorde.Money.Image (Buyer.Cash));
      Seller.Log_Trade
        ("earn "
         & Concorde.Money.Image (Concorde.Money.Total (Price, Quantity))
         & " for " & Concorde.Quantities.Image (Quantity)
         & " " & Item.Name
         & " @ " & Concorde.Money.Image (Price)
         & "; cash now " & Concorde.Money.Image (Seller.Cash));
   end Log_Transaction;

   ---------------
   -- Log_Wages --
   ---------------

   procedure Log_Wages
     (Employer : Root_Agent_Type'Class;
      Worker   : not null access constant Root_Agent_Type'Class;
      Quantity : Concorde.Quantities.Quantity_Type;
      Price    : Concorde.Money.Price_Type)
   is
   begin
      Employer.Log
        ("salary",
         "pay " & Concorde.Quantities.Image (Quantity)
         & " " & Worker.Short_Name
         & " " & Concorde.Money.Image (Price)
         & " ea; total "
         & Concorde.Money.Image (Concorde.Money.Total (Price, Quantity))
         & "; employer/worker cash now "
         & Concorde.Money.Image (Employer.Cash)
         & "/" & Concorde.Money.Image (Worker.Cash));
   end Log_Wages;

   ------------
   -- Market --
   ------------

   function Market
     (Agent : Root_Agent_Type'Class)
      return access constant Concorde.Trades.Trade_Interface'Class
   is
   begin
      return Agent.Market;
   end Market;

   ----------------------
   -- Maximum_Quantity --
   ----------------------

   overriding function Maximum_Quantity
     (Agent : Root_Agent_Type)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Agent.Stock.Maximum_Quantity;
   end Maximum_Quantity;

   -----------------------
   -- Mean_Price_Belief --
   -----------------------

   function Mean_Price_Belief
     (Agent     : Root_Agent_Type'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Concorde.Money.Price_Type
   is
      use Concorde.Money;
      Belief : constant Agent_Price_Belief_Record :=
                 Agent.Get_Price_Belief (Agent.Market, Commodity,
                                         Concorde.Trades.Ask);
   begin
      return Adjust_Price (Belief.Low + Belief.High, 0.5);
   end Mean_Price_Belief;

   -----------------------
   -- Minimum_Ask_Price --
   -----------------------

   function Minimum_Ask_Price
     (Agent     : Root_Agent_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type
   is
      use Concorde.Money;
      Agent_Class : Root_Agent_Type'Class renames
                      Root_Agent_Type'Class (Agent);
      Tax_Category     : constant Concorde.Trades.Market_Tax_Category :=
                           (if Agent_Class.Market_Resident
                            then Concorde.Trades.Sales
                            else Concorde.Trades.Import);
      Minimum_Price    : constant Price_Type :=
                           Add_Tax
                             (Agent_Class.Get_Average_Price (Commodity),
                              Agent_Class.Market.Manager.Tax_Rate
                                (Tax_Category, Commodity));
   begin
      return Minimum_Price;
   end Minimum_Ask_Price;

   ---------------
   -- New_Agent --
   ---------------

   procedure New_Agent
     (Agent          : in out Root_Agent_Type'Class;
      Location       : Concorde.Locations.Object_Location;
      Government     : access constant
        Root_Agent_Type'Class;
      Market         : access constant
        Concorde.Trades.Trade_Interface'Class;
      Cash           : Concorde.Money.Money_Type;
      Stock_Capacity : Concorde.Quantities.Quantity_Type)
   is
   begin
      Next_Ref := Next_Ref + 1;
      Agent.Agent_Ref := Next_Ref;
      Agent.Stock.Create_Stock (Stock_Capacity, False);
      Agent.Belief := new Price_Belief_Vectors.Vector;
      Agent.Location := Location;
      Agent.Market := Market;
      Agent.Cash := Cash;
      Agent.Government := Government;
      Agent.Contracted_Quantities.Create_Stock
        (Concorde.Quantities.Scale (Stock_Capacity, 10.0), True);
   end New_Agent;

   --------------------------
   -- On_Accepted_Contract --
   --------------------------

   overriding procedure On_Accepted_Contract
     (Agent    : not null access constant Root_Agent_Type;
      Contract : Concorde.Contracts.Contract_Type)
   is
      Ref : constant access Root_Agent_Type'Class :=
              Agent_Type (Agent).Variable_Reference;
   begin
      Ref.Accepted_Contracts.Append (Contract);
      Ref.Contracted_Quantities.Add_Quantity
        (Item     => Contract.Commodity,
         Quantity => Contract.Quantity,
         Value    => Contract.Total_Cost);
   end On_Accepted_Contract;

   ----------------------
   -- On_Commodity_Buy --
   ----------------------

   procedure On_Commodity_Buy
     (Agent     : in out Root_Agent_Type;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type)
   is
      Total : constant Concorde.Money.Money_Type :=
                Concorde.Money.Total (Price, Quantity);
   begin
      Agent.Log
        ("buy "
         & Concorde.Quantities.Show (Quantity)
         & " "
         & Commodity.Name
         & " @ "
         & Concorde.Money.Show (Price)
         & " total "
         & Concorde.Money.Show (Total)
         & "; room = "
         & Concorde.Quantities.Show (Agent.Available_Quantity)
         & "; cash = "
         & Concorde.Money.Show (Agent.Cash)
         & "; limit = "
         & Concorde.Money.Show (Root_Agent_Type'Class (Agent).Limit_Cash));
      Agent.Remove_Cash (Total);

      if not Commodity.Is_Set
        (Concorde.Commodities.Virtual)
        and then Quantity > Agent.Available_Quantity
      then
         Agent.Log ("insufficient room");
         for Item of Concorde.Commodities.All_Commodities loop
            if Agent.Get_Quantity (Item) > Quantities.Zero then
               Agent.Log (Quantities.Show (Agent.Get_Quantity (Item))
                          & " " & Item.Name);
            end if;
         end loop;
         Agent.Log (Quantities.Show (Agent.Total_Quantity)
                    & " total");
         Agent.Log (Quantities.Show (Agent.Maximum_Quantity)
                    & " maximum");
         raise Constraint_Error with
           "agent " & Agent.Identifier
           & " has insufficient room for purchase";
      end if;
      Agent.Add_Quantity (Commodity, Quantity, Total);
   end On_Commodity_Buy;

   -----------------------
   -- On_Commodity_Sell --
   -----------------------

   procedure On_Commodity_Sell
     (Agent     : in out Root_Agent_Type;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type)
   is
      Total : constant Concorde.Money.Money_Type :=
                Concorde.Money.Total (Price, Quantity);
   begin
      if not Commodity.Is_Pop_Group then
         if Quantity > Agent.Get_Quantity (Commodity) then
            raise Constraint_Error with
              "attempted to remove "
              & Show (Quantity)
              & " " & Commodity.Name
              & " from " & Agent.Identifier
              & " who has only "
              & Show (Agent.Get_Quantity (Commodity));
         end if;

         Agent.Remove_Quantity
           (Commodity, Quantity, Total);
      end if;
      Agent.Add_Cash (Total);
      Agent.Log
        ("sell "
         & Concorde.Quantities.Show (Quantity)
         & " "
         & Commodity.Name
         & " @ "
         & Concorde.Money.Show (Price)
         & " total "
         & Concorde.Money.Show (Total)
         & "; remaining = "
         & Show (Agent.Get_Quantity (Commodity))
         & "; cash = "
         & Concorde.Money.Show (Agent.Cash)
         & "; limit = "
         & Concorde.Money.Show (Root_Agent_Type'Class (Agent).Limit_Cash));
   end On_Commodity_Sell;

   --------------------------
   -- On_Contract_Accepted --
   --------------------------

   overriding procedure On_Contract_Accepted
     (Agent    : not null access constant Root_Agent_Type;
      Contract : Concorde.Contracts.Contract_Type)
   is
   begin
      Agent_Type (Agent)
        .Variable_Reference.Accepted_Contracts.Append (Contract);
   end On_Contract_Accepted;

   ---------------------------
   -- On_Contract_Fulfilled --
   ---------------------------

   overriding procedure On_Contract_Fulfilled
     (Agent    : not null access constant Root_Agent_Type;
      Contract : Concorde.Contracts.Contract_Type)
   is
      use type Concorde.Money.Money_Type;
      Ref : constant access Root_Agent_Type'Class :=
              Agent_Type (Agent).Variable_Reference;
   begin
      case Contract.Class is
         when Concorde.Contracts.Buy_Goods =>
            if Agent.Offered_Contract (Contract) then
               pragma Assert (Contract.Total_Cost <= Agent.Reserved_Cash);
               Agent.Log_Trade
                 ("cash " & Concorde.Money.Show (Agent.Cash)
                  & "; limit cash: " & Concorde.Money.Show (Agent.Limit_Cash)
                  & "; paying for " & Contract.Show);
               Ref.Add_Quantity
                 (Contract.Commodity, Contract.Quantity, Contract.Total_Cost);
               Ref.Reserved_Cash :=
                 Ref.Reserved_Cash - Contract.Total_Cost;
            else
               Agent.Log_Trade
                 (Contract.Commodity.Name
                  & " quantity "
                  & Concorde.Quantities.Show
                    (Agent.Get_Quantity (Contract.Commodity))
                  & "; earning for " & Contract.Show);
               Ref.Remove_Quantity
                 (Contract.Commodity, Contract.Quantity, Contract.Total_Cost);
               Ref.Add_Cash (Contract.Total_Cost);
               Ref.Contracted_Quantities.Remove_Quantity
                 (Contract.Commodity, Contract.Quantity, Contract.Total_Cost);
               if not Ref.Market_Resident then
                  Ref.Log_Trade
                    ("notifying foreign trade");
                  Ref.Market.Notify_Foreign_Trade
                    (Trader    => Ref,
                     Offer     => Concorde.Trades.Ask,
                     Commodity => Contract.Commodity,
                     Quantity  => Contract.Quantity,
                     Price     => Contract.Price);
               end if;
            end if;
--              declare
--               Accepted_Position : Current_Contract_Lists.Cursor :=
--                                   Agent.Accepted_Contracts.Find (Contract);
--                 Offered_Position  : Current_Contract_Lists.Cursor :=
--                                    Agent.Offered_Contracts.Find (Contract);
--              begin
--                 pragma Assert
--                   (Current_Contract_Lists.Has_Element (Accepted_Position)
--                    or else Current_Contract_Lists.Has_Element
--                      (Offered_Position));
--             if Current_Contract_Lists.Has_Element (Accepted_Position) then
--                    Root_Agent_Type'Class (Agent.all)
--                      .Variable_Reference.Accepted_Contracts.Delete
--                        (Accepted_Position);
--                 end if;
--            if Current_Contract_Lists.Has_Element (Offered_Position) then
--                    Root_Agent_Type'Class (Agent.all)
--                      .Variable_Reference.Offered_Contracts.Delete
--                        (Offered_Position);
--                 end if;
--              end;
      end case;
   end On_Contract_Fulfilled;

   -----------------------------
   -- Price_Position_In_Range --
   -----------------------------

   function Price_Position_In_Range
     (Value, Low, High : Concorde.Money.Price_Type)
      return Unit_Real
   is
      use Concorde.Money;
   begin
      if Value <= Low then
         return 0.0;
      elsif Value >= High then
         return 1.0;
      else
         return To_Real (Value - Low) / To_Real (High - Low);
      end if;
   end Price_Position_In_Range;

   ------------------
   -- Remove_Agent --
   ------------------

   procedure Remove_Agent
     (Agent : not null access constant Root_Agent_Type'Class)
   is
      Position : Agent_Lists.Cursor :=
                   Agent_Map.Element (Agent.Identifier);
   begin
      Agent_List.Delete (Position);
      Agent_Map.Delete (Agent.Identifier);
   end Remove_Agent;

   -----------------
   -- Remove_Cash --
   -----------------

   procedure Remove_Cash
     (Agent  : in out Root_Agent_Type'Class;
      Amount : Concorde.Money.Money_Type)
   is
      use type Concorde.Money.Money_Type;
   begin
      pragma Assert (Agent.Limit_Cash >= Amount);
      if Agent.Cash >= Amount then
         Agent.Set_Cash (Agent.Cash - Amount);
      else
         Agent.Guarantor.Variable_Reference.Remove_Cash (Amount - Agent.Cash);
         Agent.Set_Cash (Concorde.Money.Zero);
      end if;
   end Remove_Cash;

   ------------------
   -- Require_Cash --
   ------------------

   procedure Require_Cash
     (Agent  : in out Root_Agent_Type;
      Amount : Concorde.Money.Money_Type)
   is
      use Concorde.Money;
      Missing : constant Money_Type :=
                  (if Amount <= Agent.Cash
                   then Zero else Amount - Agent.Cash);
   begin
      if Missing > Zero then
         if Agent.Guarantor /= null then
            Agent.Guarantor.Variable_Reference.Require_Cash (Missing);
            if Agent.Guarantor.Cash >= Missing then
               Agent.Guarantor.Variable_Reference.Remove_Cash (Missing);
               Agent.Add_Cash (Missing);
            end if;
         end if;
      end if;
   end Require_Cash;

   ----------------
   -- Save_Agent --
   ----------------

   procedure Save_Agent
     (Agent : not null access constant Root_Agent_Type'Class)
   is
   begin
      Agent_List.Append (Agent_Type (Agent));
      Agent_Map.Insert (Agent.Identifier, Agent_List.Last);
   end Save_Agent;

   -----------------------------
   -- Scan_Accepted_Contracts --
   -----------------------------

   overriding procedure Scan_Accepted_Contracts
     (Agent   : in out Root_Agent_Type;
      Process : not null access
        procedure (Contract : Concorde.Contracts.Contract_Type))
   is
   begin
      for Contract of Agent.Accepted_Contracts loop
         if not Contract.Completed then
            Process (Contract);
         end if;
      end loop;
   end Scan_Accepted_Contracts;

   -----------------
   -- Scan_Agents --
   -----------------

   procedure Scan_Agents
     (Process : not null access procedure
        (Agent : Agent_Type))
   is
   begin
      for Agent of Agent_List loop
         Process (Agent);
      end loop;
   end Scan_Agents;

   ----------------
   -- Scan_Stock --
   ----------------

   overriding procedure Scan_Stock
     (Agent    : Root_Agent_Type;
      Process  : not null access
        procedure (Commodity : Concorde.Commodities.Commodity_Type))
   is
   begin
      Agent.Stock.Scan_Stock (Process);
   end Scan_Stock;

   --------------
   -- Set_Cash --
   --------------

   procedure Set_Cash
     (Agent  : in out Root_Agent_Type'Class;
      Amount : Concorde.Money.Money_Type)
   is
      use Concorde.Money;
   begin
      Agent.Log_Trade ("cash: " & Show (Amount));
      if Amount > Agent.Cash then
         Agent.Last_Earnings := Agent.Last_Earnings + Amount - Agent.Cash;
      else
         Agent.Last_Expenses := Agent.Last_Expenses + Agent.Cash - Amount;
      end if;
      Agent.Cash := Amount;
   end Set_Cash;

   --------------------
   -- Set_Government --
   --------------------

   procedure Set_Government
     (Agent      : in out Root_Agent_Type'Class;
      Government : not null access constant Root_Agent_Type'Class)
   is
   begin
      Agent.Government := Government;
   end Set_Government;

   -------------------
   -- Set_Guarantor --
   -------------------

   procedure Set_Guarantor
     (Agent     : in out Root_Agent_Type'Class;
      Guarantor : access constant Root_Agent_Type'Class)
   is
   begin
      Agent.Guarantor := Guarantor;
   end Set_Guarantor;

   ------------------
   -- Set_Location --
   ------------------

   overriding procedure Set_Location
     (Agent    : in out Root_Agent_Type;
      Location : Concorde.Locations.Object_Location)
   is
   begin
      Agent.Location := Location;
   end Set_Location;

   ----------------
   -- Set_Market --
   ----------------

   procedure Set_Market
     (Agent  : in out Root_Agent_Type'Class;
      Market : not null access constant
        Concorde.Trades.Trade_Interface'Class)
   is
   begin
      Agent.Market := Market;
   end Set_Market;

   ------------------
   -- Set_Quantity --
   ------------------

   overriding procedure Set_Quantity
     (Agent    : in out Root_Agent_Type;
      Item     : Concorde.Commodities.Commodity_Type;
      Quantity : Concorde.Quantities.Quantity_Type;
      Value    : Concorde.Money.Money_Type)
   is
   begin
      Agent.Stock.Set_Quantity (Item, Quantity, Value);
   end Set_Quantity;

   ---------------
   -- Translate --
   ---------------

   procedure Translate
     (Belief : in out Agent_Price_Belief_Record;
      Toward : Concorde.Money.Price_Type;
      Factor : Unit_Real)
   is
      use Concorde.Money;
      Current_Mean : constant Price_Type :=
                       Adjust_Price (Belief.High + Belief.Low, 0.5);
      New_Mean     : constant Price_Type :=
                       (if Toward > Current_Mean
                        then Adjust_Price (Toward - Current_Mean, Factor)
                        + Current_Mean
                        else Adjust_Price (Current_Mean - Toward,
                          1.0 - Factor)
                        + Toward);
      New_Low      : constant Price_Type :=
                       (if New_Mean > Current_Mean
                        or else Current_Mean - New_Mean < Belief.Low
                        then Belief.Low + New_Mean - Current_Mean
                        else Min (New_Mean, To_Price (0.01)));
      New_High      : constant Price_Type :=
                        Belief.High + New_Mean - Current_Mean;
   begin
      Belief.Low := New_Low;
      Belief.High := New_High;
   end Translate;

   -------------------
   -- Update_Agents --
   -------------------

   procedure Update_Agents
     (Update : not null access procedure
        (Agent : not null access Root_Agent_Type'Class))
   is
      procedure Local_Update
        (Rec : not null access Memor.Root_Record_Type'Class);

      ------------------
      -- Local_Update --
      ------------------

      procedure Local_Update
        (Rec : not null access Memor.Root_Record_Type'Class)
      is
      begin
         Update (Root_Agent_Type'Class (Rec.all)'Access);
      end Local_Update;

   begin
      for Agent of Agent_List loop
         Agent.Object_Database.Update (Agent.Reference, Local_Update'Access);
      end loop;
   end Update_Agents;

   -------------------------
   -- Update_Price_Belief --
   -------------------------

--     overriding procedure Update_Price_Belief
--       (Agent             : Root_Agent_Type;
--        Market            : Concorde.Trades.Trade_Interface'Class;
--        Offer             : Concorde.Trades.Offer_Type;
--        Commodity         : Concorde.Commodities.Commodity_Type;
--        Total_Traded      : Concorde.Quantities.Quantity_Type;
--        Total_Supply      : Concorde.Quantities.Quantity_Type;
--        Total_Demand      : Concorde.Quantities.Quantity_Type;
--        Average_Price     : Concorde.Money.Price_Type;
--        Historical_Price  : Concorde.Money.Price_Type;
--        Trader_Price      : Concorde.Money.Price_Type;
--        Trader_Offered    : Concorde.Quantities.Quantity_Type;
--        Trader_Traded     : Concorde.Quantities.Quantity_Type;
--        Total_Money       : Concorde.Money.Money_Type)
--     is
--        pragma Unreferenced (Historical_Price);
--        pragma Unreferenced (Total_Money);
--
--        use Concorde.Money;
--        use Concorde.Quantities;
--
--        use all type Concorde.Trades.Offer_Type;
--
--        Price_Belief   : Agent_Price_Belief_Record :=
--                           Agent.Get_Price_Belief (Market, Commodity);
--        Low            : Price_Type := Price_Belief.Low;
--        High           : Price_Type := Price_Belief.High;
--        Strength       : Unit_Real := Price_Belief.Strength;
--
--        Mean_Belief    : constant Price_Type :=
--                           Adjust_Price (High - Low, 0.5) + Low;
--        Trade_Offers   : constant Quantity_Type :=
--                           (case Offer is
--                               when Buy => Total_Demand,
--                               when Sell => Total_Supply);
--        Available_Offers : constant Quantity_Type :=
--                             (case Offer is
--                                 when Buy  => Total_Supply,
--                                 when Sell => Total_Demand);
--        Offer_Share    : constant Unit_Real :=
--                           To_Real (Trader_Offered) / To_Real (Trade_Offers);
--        Closed_Share     : constant Unit_Real :=
--                             (if Total_Traded = Zero
--                              then 0.0
--                              else To_Real (Trader_Traded)
--                              / To_Real (Total_Traded));
--
--        Competing      : constant Boolean :=
--                           Trader_Offered < Trade_Offers;
--
--     begin
--        if Closed_Share > Offer_Share
--          or else (not Competing and then Trader_Traded = Available_Offers)
--        then
--           Low := Low + Adjust_Price (Mean_Belief - Low, 0.05);
--           High := High - Adjust_Price (High - Mean_Belief, 0.05);
--           Strength := Strength + (1.0 - Strength) * 0.1;
--
--           case Offer is
--              when Buy =>
--                 if Mean_Belief > Average_Price then
--                    Low := Low
--                      - Adjust_Price (Mean_Belief - Average_Price, 0.1);
--                    High := High
--                      - Adjust_Price (Mean_Belief - Average_Price, 0.1);
--                 end if;
--              when Sell =>
--                 if Mean_Belief < Average_Price then
--                    Low := Low
--                      + Adjust_Price (Average_Price - Mean_Belief, 0.1);
--                    High := High
--                      + Adjust_Price (Average_Price - Mean_Belief, 0.1);
--                 end if;
--           end case;
--
--        elsif Closed_Share < Offer_Share
--          or else (not Competing and then Trader_Traded < Available_Offers)
--        then
--           Strength := Strength * 0.9;
--           High := Adjust_Price (High, 1.05);
--           case Offer is
--              when Buy =>
--                 if Mean_Belief < Average_Price then
--                    Low := Low
--                      + Adjust_Price (Average_Price - Mean_Belief, 0.1);
--                    High := High
--                      + Adjust_Price (Average_Price - Mean_Belief, 0.1);
--                 end if;
--              when Sell =>
--                 if not Competing then
--                    declare
--                       Share : constant Unit_Real :=
--                                 To_Real (Trader_Traded)
--                                 / To_Real (Available_Offers);
--                    begin
--                       Low := Adjust_Price
--                         (Average_Price, 0.5 + Share / 2.0);
--                    end;
--                 else
--                    Low := Adjust_Price (Low, 0.95);
--                 end if;
--           end case;
--        end if;
--
--        if Log_Price_Updates then
--           declare
--              Offered_Text : constant String :=
--                               (case Offer is
--                                   when Buy => "wanted",
--                                   when Sell => "offered");
--              Closed_Text  : constant String :=
--                               (case Offer is
--                                   when Buy => "bought",
--                                   when Sell => "sold");
--           begin
--              Agent.Log_Price
--                (Commodity.Name
--                 & ": " & Offered_Text & " "
--                 & Image (Trader_Offered)
--                 & " @ "
--                 & Image (Trader_Price)
--                 & "; " & Closed_Text & " "
--                 & Image (Trader_Traded)
--                 & "; market demand/supply/traded "
--                 & Image (Total_Demand)
--                 & "/"
--                 & Image (Total_Supply)
--                 & "/"
--                 & Image (Total_Traded)
--                 & " @ "
--                 & Image (Average_Price)
--                 & "; old price belief "
--                 & Image (Price_Belief.Low)
--                 & "/"
--                 & Image (Price_Belief.High)
--                 & "; mean "
--                 & Image (Mean_Belief)
--                 & "; new price belief "
--                 & Image (Low)
--                 & "/"
--                 & Image (High)
--                 & "; mean "
--                 & Image (Adjust_Price (Low + High, 0.5)));
--           end;
--        end if;
--
--        Price_Belief := (Low, High, Strength);
--        Agent.Update_Price_Belief (Commodity, Price_Belief);
--     end Update_Price_Belief;

   -------------------------
   -- Update_Price_Belief --
   -------------------------

   procedure Update_Price_Belief
     (Agent     : Root_Agent_Type'Class;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      Belief    :  Agent_Price_Belief_Record)
   is
   begin
      if Agent.Belief.Element (Commodity) = null then
         Agent.Belief.Replace_Element
           (Commodity,
            new Agent_Price_Belief_Record'(Belief));
      else
         Agent.Belief.Element (Commodity).all := Belief;
      end if;
   end Update_Price_Belief;

   -------------------
   -- Update_Trader --
   -------------------

   overriding procedure Update_Trader
     (Agent  : Root_Agent_Type;
      Update : not null access
        procedure (Agent : not null access
                     Concorde.Trades.Trader_Interface'Class))
   is
      procedure Do_Update (Rec : not null access
                             Memor.Root_Record_Type'Class);

      ---------------
      -- Do_Update --
      ---------------

      procedure Do_Update (Rec : not null access
                             Memor.Root_Record_Type'Class)
      is
      begin
         Update (Root_Agent_Type'Class (Rec.all)'Access);
      end Do_Update;

   begin
      Root_Agent_Type'Class (Agent).Object_Database.Update
        (Agent.Reference, Do_Update'Access);
   end Update_Trader;

end Concorde.Agents;
