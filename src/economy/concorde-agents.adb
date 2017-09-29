with Ada.Exceptions;

with Concorde.Logging;
with Concorde.Random;
with Concorde.Real_Images;

package body Concorde.Agents is

   Log_Offers : Boolean := False;
   Log_Price_Updates : constant Boolean := True;

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
      use Concorde.Money, Concorde.Quantities;

      procedure Update_Ask_Price
        (Commodity : not null access constant
           Concorde.Commodities.Root_Commodity_Type'Class;
         Offer     : Agent_Offer);

      procedure Update_Bid_Price
        (Commodity : not null access constant
           Concorde.Commodities.Root_Commodity_Type'Class;
         Offer     : Agent_Offer);

      ----------------------
      -- Update_Ask_Price --
      ----------------------

      procedure Update_Ask_Price
        (Commodity : not null access constant
           Concorde.Commodities.Root_Commodity_Type'Class;
         Offer     : Agent_Offer)
      is
      begin
         null;
      end Update_Ask_Price;

      ----------------------
      -- Update_Bid_Price --
      ----------------------

      procedure Update_Bid_Price
        (Commodity : not null access constant
           Concorde.Commodities.Root_Commodity_Type'Class;
         Offer     : Agent_Offer)
      is
      begin
         null;
      end Update_Bid_Price;

   begin
      Agent.Bids.Scan (Update_Bid_Price'Access);
      Agent.Asks.Scan (Update_Ask_Price'Access);
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

   ----------------
   -- Create_Ask --
   ----------------

   procedure Create_Ask
     (Agent        : not null access constant Root_Agent_Type'Class;
      Commodity    : Concorde.Commodities.Commodity_Type;
      Ask_Quantity : Concorde.Quantities.Quantity_Type)
   is

      procedure Update
        (Market : in out Concorde.Trades.Trade_Interface'Class);

      ------------
      -- Update --
      ------------

      procedure Update
        (Market : in out Concorde.Trades.Trade_Interface'Class)
      is
         use Concorde.Money;
         use Concorde.Quantities;
         use Concorde.Trades;

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
         Limit_Price   : constant Price_Type :=
                           Add_Tax (Agent.Get_Average_Price (Commodity),
                                    Market.Manager.Tax_Rate
                                      (Concorde.Trades.Sales,
                                       Commodity));
         Sell_Price    : constant Price_Type :=
                           (if Belief.High < Limit_Price
                            then Limit_Price
                            elsif Agent.Offer_Strategy (Commodity)
                            = Average_Price
                            then Mean
                            else Create_Ask_Price
                              (Max (Belief.Low, Limit_Price),
                               Belief.High, Agent.Age));
         Sell_Quantity : constant Quantity_Type := Ask_Quantity;
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
               & "; favourability "
               & Concorde.Real_Images.Approximate_Image (Favourability)
               & "; limit price "
               & Image (Limit_Price)
               & "; cash "
               & Image (Agent.Cash)
               & "; sell price "
               & Image (Sell_Price)
               & "; quantity "
               & Image (Sell_Quantity));
         end if;

         if Sell_Quantity > Zero then
            Market.Create_Offer
              (Offer     => Concorde.Trades.Sell,
               Trader    => Agent,
               Commodity => Commodity,
               Quantity  => Sell_Quantity);
         end if;
      end Update;

   begin
      Agent.Market.Update (Update'Access);
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

      procedure Update
        (Market : in out Concorde.Trades.Trade_Interface'Class);

      ------------
      -- Update --
      ------------

      procedure Update
        (Market : in out Concorde.Trades.Trade_Interface'Class)
      is
         use Concorde.Money;
         use Concorde.Quantities;
         use Concorde.Trades;

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
                            then Mean
                            else Create_Bid_Price
                              (Belief.Low, Belief.High, Agent.Age));
      begin
         if Log_Offers then
            Agent.Log_Trade
              (Commodity.Name
               & ": desire "
               & Image (Bid_Quantity)
               & ": have "
               & Image (Current)
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

         if Bid_Quantity > Zero then
            Market.Create_Offer
              (Offer     => Concorde.Trades.Buy,
               Trader    => Agent,
               Commodity => Commodity,
               Quantity  => Bid_Quantity);
         end if;
      end Update;

   begin
      Agent.Market.Update (Update'Access);
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
     (Agent     : in out Root_Agent_Type;
      Offer     : Concorde.Trades.Offer_Type;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Cost      : Concorde.Money.Money_Type)
   is
   begin

      case Offer is
         when Concorde.Trades.Buy =>
            Agent.Add_Quantity (Commodity, Quantity, Cost);
            Agent.Remove_Cash (Cost);
         when Concorde.Trades.Sell =>
            Agent.Remove_Quantity (Commodity, Quantity, Cost);
            Agent.Add_Cash (Cost);
      end case;

      Agent.Account.Append
        ((Date       => Concorde.Dates.Current_Date,
          Item       => Commodity,
          Entry_Type => Offer,
          Quantity   => Quantity,
          Cost       => Cost,
          Balance    => Agent.Cash));

   end Execute_Trade;

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
      Market    : Concorde.Trades.Trade_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
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

   ----------------------------
   -- Maximum_Offer_Quantity --
   ----------------------------

   overriding function Maximum_Offer_Quantity
     (Agent  : Root_Agent_Type;
      Offer  : Concorde.Trades.Offer_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
   is
      use type Concorde.Quantities.Quantity_Type;
   begin
      case Offer is
         when Concorde.Trades.Buy =>
            return Agent.Maximum_Quantity - Agent.Total_Quantity;
         when Concorde.Trades.Sell =>
            return Agent.Get_Quantity (Commodity);
      end case;
   end Maximum_Offer_Quantity;

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

   overriding procedure Update_Price_Belief
     (Agent             : Root_Agent_Type;
      Market            : Concorde.Trades.Trade_Interface'Class;
      Offer             : Concorde.Trades.Offer_Type;
      Commodity         : Concorde.Commodities.Commodity_Type;
      Total_Traded      : Concorde.Quantities.Quantity_Type;
      Total_Supply      : Concorde.Quantities.Quantity_Type;
      Total_Demand      : Concorde.Quantities.Quantity_Type;
      Average_Price     : Concorde.Money.Price_Type;
      Historical_Price  : Concorde.Money.Price_Type;
      Trader_Price      : Concorde.Money.Price_Type;
      Trader_Offered    : Concorde.Quantities.Quantity_Type;
      Trader_Traded     : Concorde.Quantities.Quantity_Type;
      Total_Money       : Concorde.Money.Money_Type)
   is
      pragma Unreferenced (Historical_Price);
      pragma Unreferenced (Total_Money);

      use Concorde.Money;
      use Concorde.Quantities;

      use all type Concorde.Trades.Offer_Type;

      Price_Belief   : Agent_Price_Belief_Record :=
                         Agent.Get_Price_Belief (Market, Commodity);
      Low            : Price_Type := Price_Belief.Low;
      High           : Price_Type := Price_Belief.High;
      Strength       : Unit_Real := Price_Belief.Strength;

      Mean_Belief    : constant Price_Type :=
                         Adjust_Price (High - Low, 0.5) + Low;
      Trade_Offers   : constant Quantity_Type :=
                         (case Offer is
                             when Buy => Total_Demand,
                             when Sell => Total_Supply);
      Available_Offers : constant Quantity_Type :=
                           (case Offer is
                               when Buy  => Total_Supply,
                               when Sell => Total_Demand);
      Offer_Share    : constant Unit_Real :=
                         To_Real (Trader_Offered) / To_Real (Trade_Offers);
      Closed_Share     : constant Unit_Real :=
                           (if Total_Traded = Zero
                            then 0.0
                            else To_Real (Trader_Traded)
                            / To_Real (Total_Traded));

      Competing      : constant Boolean :=
                         Trader_Offered < Trade_Offers;

   begin
      if Closed_Share > Offer_Share
        or else (not Competing and then Trader_Traded = Available_Offers)
      then
         Low := Low + Adjust_Price (Mean_Belief - Low, 0.05);
         High := High - Adjust_Price (High - Mean_Belief, 0.05);
         Strength := Strength + (1.0 - Strength) * 0.1;

         case Offer is
            when Buy =>
               if Mean_Belief > Average_Price then
                  Low := Low
                    - Adjust_Price (Mean_Belief - Average_Price, 0.1);
                  High := High
                    - Adjust_Price (Mean_Belief - Average_Price, 0.1);
               end if;
            when Sell =>
               if Mean_Belief < Average_Price then
                  Low := Low
                    + Adjust_Price (Average_Price - Mean_Belief, 0.1);
                  High := High
                    + Adjust_Price (Average_Price - Mean_Belief, 0.1);
               end if;
         end case;

      elsif Closed_Share < Offer_Share
        or else (not Competing and then Trader_Traded < Available_Offers)
      then
         Strength := Strength * 0.9;
         High := Adjust_Price (High, 1.05);
         case Offer is
            when Buy =>
               if Mean_Belief < Average_Price then
                  Low := Low
                    + Adjust_Price (Average_Price - Mean_Belief, 0.1);
                  High := High
                    + Adjust_Price (Average_Price - Mean_Belief, 0.1);
               end if;
            when Sell =>
               if not Competing then
                  declare
                     Share : constant Unit_Real :=
                               To_Real (Trader_Traded)
                               / To_Real (Available_Offers);
                  begin
                     Low := Adjust_Price
                       (Average_Price, 0.5 + Share / 2.0);
                  end;
               else
                  Low := Adjust_Price (Low, 0.95);
               end if;
         end case;
      end if;

      if Log_Price_Updates then
         declare
            Offered_Text : constant String :=
                             (case Offer is
                                 when Buy => "wanted",
                                 when Sell => "offered");
            Closed_Text  : constant String :=
                             (case Offer is
                                 when Buy => "bought",
                                 when Sell => "sold");
         begin
            Agent.Log_Price
              (Commodity.Name
               & ": " & Offered_Text & " "
               & Image (Trader_Offered)
               & " @ "
               & Image (Trader_Price)
               & "; " & Closed_Text & " "
               & Image (Trader_Traded)
               & "; market demand/supply/traded "
               & Image (Total_Demand)
               & "/"
               & Image (Total_Supply)
               & "/"
               & Image (Total_Traded)
               & " @ "
               & Image (Average_Price)
               & "; old price belief "
               & Image (Price_Belief.Low)
               & "/"
               & Image (Price_Belief.High)
               & "; mean "
               & Image (Mean_Belief)
               & "; new price belief "
               & Image (Low)
               & "/"
               & Image (High)
               & "; mean "
               & Image (Adjust_Price (Low + High, 0.5)));
         end;
      end if;

      Price_Belief := (Low, High, Strength);
      Agent.Update_Price_Belief (Commodity, Price_Belief);
   end Update_Price_Belief;

   -------------------------
   -- Update_Price_Belief --
   -------------------------

   procedure Update_Price_Belief
     (Agent     : Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
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
