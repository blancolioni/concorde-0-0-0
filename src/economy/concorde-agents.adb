with Ada.Exceptions;

with Concorde.Logging;
with Concorde.Random;

package body Concorde.Agents is

   Log_Offers : Boolean := False;

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
   -- Create_Buy_Offer --
   ----------------------

   procedure Create_Buy_Offer
     (Agent     : not null access constant Root_Agent_Type'Class;
      Market    : in out Concorde.Trades.Trade_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Desired   : Concorde.Quantities.Quantity;
      Minimum   : Concorde.Quantities.Quantity)
   is
      use Concorde.Money;
      use Concorde.Quantities;

      Current       : constant Quantity :=
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
                        (if Minimum = Zero
                         then Belief.Low
                         else Create_Bid_Price
                           (Belief.Low, Belief.High, Agent.Age));
      Limit_Price   : constant Price_Type := Belief.High;
      Favoured      : constant Quantity :=
                        (if Current >= Desired
                         then Zero
                         elsif Current >= Minimum
                         then Scale (Desired - Minimum, Favourability)
                         else Minimum
                         + Scale (Desired - Minimum, Favourability));
      Possible      : constant Quantity :=
                        Max (Get_Quantity (Agent.Limit_Cash, Buy_Price),
                             Zero);
      Final         : constant Quantity :=
                        Min (Favoured, Possible);
   begin
      if Log_Offers then
         Agent.Log_Trade
           (Commodity.Name
            & ": desire "
            & Image (Desired)
            & ": minimum "
            & Image (Minimum)
            & ": have "
            & Image (Current)
            & ": possible "
            & Image (Possible)
            & "; price belief "
            & Image (Belief.Low)
            & "/"
            & Image (Belief.High)
            & "; mean "
            & Image (Mean)
            & "; favourability "
            & Lui.Approximate_Image (Favourability)
            & "; buy price "
            & Image (Buy_Price)
            & "; limit price "
            & Image (Limit_Price)
            & "; cash "
            & Image (Agent.Cash)
            & "; quantity "
            & Image (Final));
      end if;

      if Final > Zero then
         Market.Create_Offer
           (Offer     => Concorde.Trades.Buy,
            Trader    => Agent,
            Commodity => Commodity,
            Quantity  => Final,
            Price     => Buy_Price,
            Limit     => Limit_Price);
      end if;
   exception
      when E : others =>
         Agent.Log_Trade
           ("while creating bid for " & Commodity.Name
            & ": "
            & Ada.Exceptions.Exception_Message (E));
   end Create_Buy_Offer;

   -----------------------
   -- Create_Sell_Offer --
   -----------------------

   procedure Create_Sell_Offer
     (Agent     : not null access constant Root_Agent_Type'Class;
      Market    : in out Concorde.Trades.Trade_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Available : Concorde.Quantities.Quantity;
      Minimum   : Concorde.Money.Money_Type)
   is
      pragma Unreferenced (Minimum);
      use Concorde.Money;
      use Concorde.Quantities;

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
      Sell_Price    : constant Price_Type :=
                        Create_Ask_Price
                          (Belief.Low, Belief.High, Agent.Age);
      Minimum_Ask   : constant Price_Type := Belief.Low;
      Sell_Quantity : constant Quantity := Available;
   begin
      if Log_Offers then
         Agent.Log_Trade
           (Commodity.Name
            & ": have "
            & Image (Available)
            & "; price belief "
            & Image (Belief.Low)
            & "/"
            & Image (Belief.High)
            & "; mean "
            & Image (Mean)
            & "; favourability "
            & Lui.Approximate_Image (Favourability)
            & "; sell price "
            & Image (Sell_Price)
            & "; limit price "
            & Image (Minimum_Ask)
            & "; cash "
            & Image (Agent.Cash)
            & "; quantity "
            & Image (Sell_Quantity));
      end if;

      if Sell_Quantity > Zero then
         Market.Create_Offer
           (Offer     => Concorde.Trades.Sell,
            Trader    => Agent,
            Commodity => Commodity,
            Quantity  => Sell_Quantity,
            Price     => Sell_Price,
            Limit     => Minimum_Ask);
      end if;
   exception
      when E : others =>
         Agent.Log_Trade
           ("while creating bid for " & Commodity.Name
            & ": "
            & Ada.Exceptions.Exception_Message (E));
   end Create_Sell_Offer;

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
     (Agent     : Root_Agent_Type;
      Offer     : Concorde.Trades.Offer_Type;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity;
      Cost      : Concorde.Money.Money_Type)
   is
      procedure Update
        (Object : in out Memor.Root_Record_Type'Class);

      ------------
      -- Update --
      ------------

      procedure Update
        (Object : in out Memor.Root_Record_Type'Class)
      is
         Agent : Root_Agent_Type'Class renames
                   Root_Agent_Type'Class (Object);
      begin
         case Offer is
            when Concorde.Trades.Buy =>
               Agent.Add_Quantity (Commodity, Quantity, Cost);
               Agent.Remove_Cash (Cost);
            when Concorde.Trades.Sell =>
               Agent.Remove_Quantity (Commodity, Quantity, Cost);
               Agent.Add_Cash (Cost);
         end case;
      end Update;

   begin

      Root_Agent_Type'Class (Agent).Object_Database.Update
        (Agent.Reference, Update'Access);

   end Execute_Trade;

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
            if Agent.Belief.Element (Commodity.Reference) = null then
               declare
                  Base : constant Price_Type :=
                           Market.Historical_Mean_Price (Commodity);
               begin
                  return (Low => Adjust_Price (Base, 0.95),
                          High => Adjust_Price (Base, 1.05),
                          Strength => 0.5);
               end;
            else
               return Agent.Belief.Element (Commodity.Reference).all;
            end if;
         when Fixed_Price =>
            return (Low => Commodity.Base_Price,
                    High => Commodity.Base_Price,
                    Strength => 1.0);
         when Average_Price =>
            return (Low      => Market.Current_Price (Commodity),
                    High     => Market.Current_Price (Commodity),
                    Strength => 1.0);
      end case;
   end Get_Price_Belief;

   ------------------
   -- Get_Quantity --
   ------------------

   overriding function Get_Quantity
     (Agent : Root_Agent_Type;
      Item  : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity
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

   function Location
     (Agent : Root_Agent_Type'Class)
      return access constant Agent_Location_Interface'Class
   is
   begin
      return Agent.Location;
   end Location;

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
        (Agent.Short_Name, Agent.Location.Agent_Location_Name,
         Category, Message);
   end Log;

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

   ----------------------------
   -- Maximum_Offer_Quantity --
   ----------------------------

   overriding function Maximum_Offer_Quantity
     (Agent  : Root_Agent_Type;
      Offer  : Concorde.Trades.Offer_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity
   is
      use type Concorde.Quantities.Quantity;
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
      return Concorde.Quantities.Quantity
   is
   begin
      return Agent.Stock.Maximum_Quantity;
   end Maximum_Quantity;

   ---------------
   -- New_Agent --
   ---------------

   procedure New_Agent
     (Agent          : in out Root_Agent_Type'Class;
      Location       : not null access constant Agent_Location_Interface'Class;
      Stock_Capacity : Concorde.Quantities.Quantity)
   is
   begin
      Agent.Stock.Create_Stock (Stock_Capacity);
      Agent.Belief := new Price_Belief_Vectors.Vector;
      Agent.Location := Location;
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

   procedure Set_Location
     (Agent    : in out Root_Agent_Type'Class;
      Location : not null access constant Agent_Location_Interface'Class)
   is
   begin
      Agent.Location := Location;
   end Set_Location;

   ------------------
   -- Set_Quantity --
   ------------------

   overriding procedure Set_Quantity
     (Agent    : in out Root_Agent_Type;
      Item     : Concorde.Commodities.Commodity_Type;
      Quantity : Concorde.Quantities.Quantity;
      Value    : Concorde.Money.Money_Type)
   is
   begin
      Agent.Stock.Set_Quantity (Item, Quantity, Value);
   end Set_Quantity;

end Concorde.Agents;
