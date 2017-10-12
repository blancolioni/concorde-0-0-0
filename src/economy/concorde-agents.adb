with Ada.Exceptions;

with Concorde.Logging;
with Concorde.Logs;
with Concorde.Random;
with Concorde.Real_Images;

package body Concorde.Agents is

   Log_Offers : Boolean := False;
--   Log_Price_Updates : constant Boolean := True;

   function Price_Position_In_Range
     (Value, Low, High : Concorde.Money.Price_Type)
      return Unit_Real;

   function Create_Ask_Price
     (Low, High : Concorde.Money.Price_Type;
      Agent_Age : Natural)
      return Concorde.Money.Price_Type;
   --  Come up with an asking price between Low and High.
   --  If the Agent_Age is low, skew toward Low

   function Create_Bid_Price
     (Low, High : Concorde.Money.Price_Type;
      Agent_Age : Natural)
      return Concorde.Money.Price_Type;
   --  Come up with an bidding price between Low and High.
   --  If the Agent_Age is low, skew toward High.

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

   ----------
   -- Cash --
   ----------

   function Cash
     (Agent : Root_Agent_Type'Class)
      return Concorde.Money.Money_Type
   is
   begin
      return Agent.Cash;
   end Cash;

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
      begin
         null;
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
         if Agent.Belief_Based_Strategy (Commodity)
           and then Offer.Quantity > Zero
         then
            declare
               Mean   : constant Price_Type :=
                          Agent.Market.Historical_Mean_Price (Commodity);
               Start_Belief : constant Agent_Price_Belief_Record :=
                                Agent.Get_Price_Belief
                                  (Agent.Market, Commodity);
               Belief       : Agent_Price_Belief_Record := Start_Belief;
               Supply : constant Quantity_Type :=
                          Agent.Market.Get_Daily_Quantity
                            (Commodity, Concorde.Trades.Local_Supply, 1);
               Demand : constant Quantity_Type :=
                          Agent.Market.Get_Daily_Quantity
                            (Commodity, Concorde.Trades.Local_Demand, 1);
            begin

               Agent.Log_Trade
                 (Commodity.Name
                  & ": checking offers;"
                  & " current price belief "
                  & Image (Belief.Low)
                  & "/"
                  & Image (Belief.High)
                  & "; mean "
                  & Image (Mean)
                  & "; supply/demand "
                  & Image (Supply)
                  & "/"
                  & Image (Demand)
                  & ": wanted " & Image (Offer.Quantity)
                  & "; got " & Image (Offer.Filled)
                  & " at "
                  & Image (Offer.Price));

               if Offer.Filled = Offer.Quantity then
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
                  & Image (Belief.Low)
                  & "/"
                  & Image (Belief.High)
                  & "; mean "
                  & Image (Adjust_Price (Belief.Low + Belief.High, 0.5)));

               declare
                  Log_Path : constant String :=
                               Agent.Short_Name
                               & "/" & Commodity.Identifier
                               & "/update-bid";
                  Log_Line : constant String :=
                               Image (Start_Belief.Low)
                             & "," & Image (Start_Belief.High)
                             & "," & Image (Mean)
                             & "," & Image (Supply)
                             & "," & Image (Demand)
                             & "," & Image (Offer.Quantity)
                             & "," & Image (Offer.Filled)
                             & "," & Image (Offer.Price)
                               & "," & Image (Belief.Low)
                               & "," & Image (Belief.High)
                               & "," & Image
                                 (Adjust_Price
                                    (Belief.Low + Belief.High, 0.5));
               begin
                  Concorde.Logs.Log_Line (Log_Path, Log_Line);
               end;

               Agent.Update_Price_Belief (Commodity, Belief);
            end;
         end if;

      end Update_Bid;

   begin
      Agent.Age := Agent.Age + 1;
      Agent.Bids.Update (Update_Bid'Access);
      Agent.Asks.Update (Update_Ask'Access);
   end Check_Offers;

   -----------------
   -- Clear_Stock --
   -----------------

   overriding procedure Clear_Stock
     (Agent    : in out Root_Agent_Type)
   is
   begin
      Agent.Stock.Clear_Stock;
   end Clear_Stock;

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

      use Concorde.Money;
      use Concorde.Trades;

      Market : constant access constant
        Concorde.Trades.Trade_Interface'Class :=
          Agent.Market;

      Mean          : constant Price_Type :=
                        Market.Historical_Mean_Price
                          (Commodity);
      Belief        : constant Agent_Price_Belief_Record :=
                        Agent.Get_Price_Belief (Market, Commodity);
      Favourability : constant Unit_Real :=
                        (if Belief.Low = Belief.High
                         then 1.0
                         else Price_Position_In_Range
                           (Mean, Belief.Low, Belief.High));
      Minimum_Price : constant Price_Type :=
                        Agent.Get_Average_Price (Commodity);

--        Limit_Price   : constant Price_Type :=
--                          Add_Tax (Agent.Get_Average_Price (Commodity),
--                                   Market.Manager.Tax_Rate
--                                     (Concorde.Trades.Sales,
--                                      Commodity));
      Sell_Price    : constant Price_Type :=
                        (if Agent.Offer_Strategy (Commodity)
                         = Average_Price
                         then Max (Mean, Minimum_Price)
                         else Create_Ask_Price
                           (Max (Minimum_Price, Belief.Low),
                            Max (Minimum_Price, Belief.High), Agent.Age));
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
         A.Ask_Quantity := A.Ask_Quantity + Sell_Quantity;
         A.Asks.Update_Element (Commodity, Update_Offer'Access);
      end Update_Agent;

      ------------------
      -- Update_Offer --
      ------------------

      procedure Update_Offer
        (Offer : in out Agent_Offer)
      is
      begin
         Offer.Quantity := Offer.Quantity + Sell_Quantity;
      end Update_Offer;

   begin
      if Log_Offers then
         Agent.Log_Trade
           (Commodity.Name
            & ": have "
            & Image (Ask_Quantity)
            & "; price belief "
            & Image (Belief.Low)
            & "/"
            & Image (Belief.High)
            & "; mean "
            & Image (Mean)
            & "; minimum "
            & Image (Minimum_Price)
            & "; favourability "
            & Concorde.Real_Images.Approximate_Image (Favourability)
            & "; cash "
            & Image (Agent.Cash)
            & "; sell price "
            & Image (Sell_Price)
            & "; quantity "
            & Image (Sell_Quantity));
      end if;

      declare
         Log_Path : constant String :=
                      Agent.Short_Name
                      & "/" & Commodity.Identifier
                      & "/ask";
         Log_Line : constant String :=
                      Image (Belief.Low)
                      & "," & Image (Belief.High)
                      & "," & Image (Mean)
                      & "," & Image (Ask_Quantity)
                      & "," & Image (Minimum_Price)
                      & "," & Image (Agent.Cash)
                      & "," & Image (Sell_Price)
                      & "," & Image (Sell_Quantity);
      begin
         Concorde.Logs.Log_Line (Log_Path, Log_Line);
      end;

      if Sell_Quantity > Zero then
         Update_Agent (Agent.Variable_Reference);
         Market.Create_Offer
           (Offer     => Concorde.Trades.Ask,
            Trader    => Agent,
            Commodity => Commodity,
            Quantity  => Sell_Quantity,
            Price     => Sell_Price);

      end if;

   exception
      when E : others =>
         Agent.Log_Trade
           ("while creating bid for " & Commodity.Name
            & ": "
            & Ada.Exceptions.Exception_Message (E));
   end Create_Ask;

   ----------------------
   -- Create_Ask_Price --
   ----------------------

   function Create_Ask_Price
     (Low, High : Concorde.Money.Price_Type;
      Agent_Age : Natural)
      return Concorde.Money.Price_Type
   is
      use Concorde.Money;
      Skew : constant Unit_Real :=
               (if Agent_Age < 5
                then 0.5 + Real (Agent_Age) / 10.0
                else 1.0);
      Factor : constant Unit_Real :=
                 Skew * Concorde.Random.Unit_Random;
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

      use Concorde.Money;
      use Concorde.Trades;

      Market : constant access constant
        Concorde.Trades.Trade_Interface'Class :=
          Agent.Market;

      Current       : constant Quantity_Type :=
                        Agent.Get_Quantity (Commodity);
      Mean          : constant Price_Type :=
                        Market.Historical_Mean_Price
                          (Commodity);
      Belief        : constant Agent_Price_Belief_Record :=
                        Agent.Get_Price_Belief (Market, Commodity);
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
                           (Belief.Low, Belief.High, Agent.Age));
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
         A.Bid_Quantity := A.Bid_Quantity + Bid_Quantity;
         A.Bids.Update_Element (Commodity, Update_Offer'Access);
      end Update_Agent;

      ------------------
      -- Update_Offer --
      ------------------

      procedure Update_Offer
        (Offer : in out Agent_Offer)
      is
      begin
         Offer.Quantity := Offer.Quantity + Bid_Quantity;
         Offer.Price := Buy_Price;
      end Update_Offer;

   begin
      if Log_Offers then
         Agent.Log_Trade
           (Commodity.Name
            & ": desire "
            & Image (Bid_Quantity)
            & ": have "
            & Image (Current)
            & "; cash "
            & Image (Agent.Cash)
            & "; price belief "
            & Image (Belief.Low)
            & "/"
            & Image (Belief.High)
            & "; mean "
            & Image (Mean)
            & "; favourability "
            & Concorde.Real_Images.Approximate_Image (Favourability)
            & "; buy price "
            & Image (Buy_Price));
      end if;

      declare
         Log_Path : constant String :=
                      Agent.Short_Name
                      & "/" & Commodity.Identifier
                      & "/bid";
         Log_Line : constant String :=
                      Image (Belief.Low)
                    & "," & Image (Belief.High)
                    & "," & Image (Mean)
                    & "," & Image (Bid_Quantity)
                    & "," & Image (Agent.Cash)
                    & "," & Image (Buy_Price)
                    & "," & Image (Bid_Quantity);
      begin
         Concorde.Logs.Log_Line (Log_Path, Log_Line);
      end;

      if Bid_Quantity > Zero then
         Update_Agent (Agent.Variable_Reference);
         Market.Create_Offer
           (Offer     => Concorde.Trades.Bid,
            Trader    => Agent,
            Commodity => Commodity,
            Quantity  => Bid_Quantity,
            Price     => Buy_Price);
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
     (Low, High : Concorde.Money.Price_Type;
      Agent_Age : Natural)
      return Concorde.Money.Price_Type
   is
      use Concorde.Money;
      Skew   : constant Unit_Real :=
                 (if Agent_Age < 5
                  then 0.5 + Real (Agent_Age) / 10.0
                  else 1.0);
      Factor : constant Unit_Real :=
                 Skew * Concorde.Random.Unit_Random + 1.0 - Skew;
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
      return Info.Quantity - Info.Filled;
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
      return Info.Quantity - Info.Filled;
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
         A.Remove_Quantity (Commodity, Quantity, Cost);
         A.Add_Cash (Cost);
         A.Asks.Update_Element (Commodity, Update_Offer'Access);
         A.Ask_Quantity := A.Ask_Quantity - Quantity;
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
         A.Bid_Quantity := A.Bid_Quantity - Quantity;
      end Execute_Bid;

      ------------------
      -- Update_Offer --
      ------------------

      procedure Update_Offer
        (Offer : in out Agent_Offer)
      is
      begin
         Offer.Filled := Offer.Filled + Quantity;
      end Update_Offer;

   begin

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
      Belief.Low := Adjust_Price (Belief.Low, 1.0 - Factor);
      Belief.High := Adjust_Price (Belief.High, 1.0 + Factor);
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
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Agent_Price_Belief_Record
   is
      use Concorde.Money;
      use all type Concorde.Trades.Offer_Price_Strategy;
   begin
      case Agent.Offer_Strategy (Commodity) is
         when Belief_Based =>
            if Agent.Belief.Element (Commodity) = null then
               declare
                  Base : constant Price_Type :=
                           Market.Historical_Mean_Price (Commodity);
               begin
                  return (Low => Adjust_Price (Base, 0.95),
                          High => Adjust_Price (Base, 1.05),
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
     (Agent : Root_Agent_Type;
      Item  : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Agent.Stock.Get_Quantity (Item);
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
        (Agent.Short_Name, Concorde.Locations.Short_Name (Agent.Location),
         Category, Message);
   end Log;

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

   ---------------
   -- New_Agent --
   ---------------

   procedure New_Agent
     (Agent          : in out Root_Agent_Type'Class;
      Location       : Concorde.Locations.Object_Location;
      Market         : access constant
        Concorde.Trades.Trade_Interface'Class;
      Stock_Capacity : Concorde.Quantities.Quantity_Type)
   is
   begin
      Agent.Stock.Create_Stock (Stock_Capacity);
      Agent.Belief := new Price_Belief_Vectors.Vector;
      Agent.Location := Location;
      Agent.Market := Market;
   end New_Agent;

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
      Agent.Set_Cash (Agent.Cash - Amount);
   end Remove_Cash;

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
   begin
      Agent.Cash := Amount;
   end Set_Cash;

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
                        else Adjust_Price (Current_Mean - Toward, 1.0 - Factor)
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
