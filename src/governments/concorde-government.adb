with Concorde.Factions;

package body Concorde.Government is

   -------------------------
   -- Add_Income_Tax_Rate --
   -------------------------

   procedure Add_Income_Tax_Rate
     (Government  : in out Root_Government_Type'Class;
      Lower_Bound : Concorde.Money.Price_Type;
      Rate        : Unit_Real)
   is
      use Concorde.Money;
      use Income_Tax_Lists;
      Position : Cursor := Government.Income_Tax_Rates.First;
   begin
      while Has_Element (Position)
        and then Element (Position).Threshold <= Lower_Bound
      loop
         Next (Position);
      end loop;
      if not Has_Element (Position) then
         Government.Income_Tax_Rates.Append ((Lower_Bound, Rate));
      elsif Element (Position).Threshold = Lower_Bound then
         Government.Income_Tax_Rates.Replace_Element
           (Position, ((Lower_Bound, Rate)));
      else
         Government.Income_Tax_Rates.Insert
           (Position, ((Lower_Bound, Rate)));
      end if;
   end Add_Income_Tax_Rate;

   ---------------
   -- Add_Power --
   ---------------

   overriding procedure Add_Power
     (Government : in out Root_Government_Type;
      Power      : Concorde.Powers.Power_Type)
   is
   begin
      Government.Powers.Add_Power (Power);
   end Add_Power;

   ----------------------
   -- Add_Trade_Offers --
   ----------------------

--     overriding procedure Add_Trade_Offers
--       (Item   : not null access constant Root_Government_Type)
--     is
--        pragma Unreferenced (Item);
--     begin
--        null;
--     end Add_Trade_Offers;

   --------------------
   -- Get_Government --
   --------------------

   function Get_Government
     (Location : Concorde.Locations.Object_Location)
      return Government_Type
   is
      use Concorde.Objects;
      Primary : constant Concorde.Objects.Object_Type :=
                  Concorde.Locations.Primary (Location);
   begin
      if Primary /= null
        and then Primary.all in Concorde.Government.Governed_Interface'Class
      then
         return Concorde.Government.Governed_Interface'Class (Primary.all)
           .Government;
      else
         return null;
      end if;
   end Get_Government;

   --------------
   -- Governed --
   --------------

   function Governed
     (Government : Root_Government_Type'Class)
      return access constant Governed_Interface'Class
   is
   begin
      return Government.Governed;
   end Governed;

   ------------------
   -- Headquarters --
   ------------------

   function Headquarters
     (Government : Root_Government_Type'Class)
      return Concorde.Installations.Installation_Type
   is
   begin
      return Government.Headquarters;
   end Headquarters;

   ----------------
   -- Income_Tax --
   ----------------

   function Income_Tax
     (Government  : Root_Government_Type'Class;
      Income      : Concorde.Money.Money_Type;
      Quantity    : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Money_Type
   is
      use Concorde.Money;
      Per_Capita : constant Price_Type := Price (Income, Quantity);
      Total_Tax  : Money_Type := Zero;
   begin
      for Tax of Government.Income_Tax_Rates loop
         exit when Tax.Threshold > Per_Capita;
         Total_Tax := Total_Tax
           + Total
           (Adjust_Price (Per_Capita - Tax.Threshold, Tax.Rate), Quantity);
      end loop;
      return Total_Tax;
   end Income_Tax;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Government : Root_Government_Type)
      return Memor.Memor_Database
   is
      pragma Unreferenced (Government);
   begin
      return Db.Get_Database;
   end Object_Database;

   ------------------
   -- Remove_Power --
   ------------------

   overriding procedure Remove_Power
     (Government : in out Root_Government_Type;
      Power      : Concorde.Powers.Power_Type)
   is
   begin
      Government.Powers.Remove_Power (Power);
   end Remove_Power;

   -----------------
   -- Scan_Powers --
   -----------------

   overriding procedure Scan_Powers
     (Government : Root_Government_Type;
      Process    : not null access
        procedure (Power : Concorde.Powers.Power_Type))
   is
   begin
      Government.Powers.Scan_Powers (Process);
   end Scan_Powers;

   -----------------------
   -- Set_Base_Tax_Rate --
   -----------------------

   procedure Set_Base_Tax_Rate
     (Government : in out Root_Government_Type'Class;
      Category   : Concorde.Trades.Market_Tax_Category;
      Rate       : Unit_Real)
   is
   begin
      Government.Base_Tax_Rate (Category) := Rate;
   end Set_Base_Tax_Rate;

   ---------------------------
   -- Set_Basic_Living_Wage --
   ---------------------------

   procedure Set_Basic_Living_Wage
     (Government : in out Root_Government_Type'Class;
      Wage       : Concorde.Money.Price_Type)
   is
   begin
      Government.Log
        ("basic living wage is "
         & Concorde.Money.Show (Wage) & "/day");
      Government.Basic_Living_Wage := Wage;
   end Set_Basic_Living_Wage;

   ------------------
   -- Set_Governor --
   ------------------

   procedure Set_Governor
     (Government : in out Root_Government_Type'Class;
      Governor   : access constant
        Concorde.People.Individuals.Root_Individual_Type'Class)
   is
   begin
      Government.Governor := Governor;
   end Set_Governor;

   ------------------
   -- Set_Tax_Rate --
   ------------------

   procedure Set_Tax_Rate
     (Government : in out Root_Government_Type'Class;
      Category   : Concorde.Trades.Market_Tax_Category;
      Commodity  : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      Rate       : Unit_Real)
   is
      Rates : Array_Of_Tax_Rates := Government.Tax_Rates.Element (Commodity);
   begin
      Rates (Category) := Rate;
      Government.Tax_Rates.Replace_Element (Commodity, Rates);
   end Set_Tax_Rate;

   --------------
   -- Tax_Rate --
   --------------

   overriding function Tax_Rate
     (Government : Root_Government_Type;
      Category   : Concorde.Trades.Market_Tax_Category;
      Commodity  : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Unit_Real
   is
      Rate : constant Unit_Real :=
               Government.Tax_Rates.Element (Commodity) (Category);
   begin
      if Rate = 0.0 then
         return Government.Base_Tax_Rate (Category);
      else
         return Rate;
      end if;
   end Tax_Rate;

   -----------------
   -- Tax_Receipt --
   -----------------

   overriding procedure Tax_Receipt
     (Government : Root_Government_Type;
      Commodity  : Concorde.Commodities.Commodity_Type;
      Quantity   : Concorde.Quantities.Quantity_Type;
      Price      : Concorde.Money.Price_Type;
      Category   : Concorde.Trades.Market_Tax_Category;
      Receipt    : Concorde.Money.Money_Type)
   is
      use Concorde.Money;
      Tithe      : constant Money_Type :=
                     Tax (Receipt, Government.Owner_Tithe);
   begin

      Government.Update.Add_Cash (Receipt - Tithe);
      Government.Update.Tax_Receipts (Category) :=
        Government.Tax_Receipts (Category) + Receipt;
      Government.Owner.Variable_Reference.Add_Cash (Tithe);

      Government.Log_Trade
        ("from sale of "
         & Concorde.Quantities.Image (Quantity)
         & " "
         & Commodity.Name
         & " @ "
         & Concorde.Money.Image (Price)
         & ", tax receipt is "
         & Concorde.Money.Image (Receipt)
         & ", tithe " & Image (Tithe)
         & ", total " & Image (Receipt - Tithe)
         & "; cash: " & Image (Government.Cash));
   end Tax_Receipt;

   ------------------
   -- Tax_Receipts --
   ------------------

   function Tax_Receipts
     (Government : Root_Government_Type'Class;
      Category   : Concorde.Trades.Market_Tax_Category)
      return Concorde.Money.Money_Type
   is
   begin
      return Government.Tax_Receipts (Category);
   end Tax_Receipts;

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant Root_Government_Type'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update;

   ------------------
   -- Update_Agent --
   ------------------

   overriding procedure Update_Agent
     (Government     : not null access constant Root_Government_Type;
      Perform_Update : not null access
        procedure (Agent : in out Concorde.Agents.Root_Agent_Type'Class))
   is
   begin
      Perform_Update (Government.Update);
   end Update_Agent;

end Concorde.Government;
