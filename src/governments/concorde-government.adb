with Concorde.Factions;

package body Concorde.Government is

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

   ------------
   -- Insert --
   ------------

   overriding procedure Insert
     (Government : in out Root_Government_Type;
      Power      : Concorde.Powers.Power_Type)
   is
   begin
      Government.Powers.Insert (Power);
   end Insert;

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

   ------------
   -- Remove --
   ------------

   overriding procedure Remove
     (Government : in out Root_Government_Type;
      Power      : Concorde.Powers.Power_Type)
   is
   begin
      Government.Powers.Remove (Power);
   end Remove;

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
      Wage       : WL.Money.Price_Type)
   is
   begin
      Government.Log
        ("basic living wage is "
         & WL.Money.Show (Wage) & "/day");
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
      Quantity   : WL.Quantities.Quantity_Type;
      Price      : WL.Money.Price_Type;
      Category   : Concorde.Trades.Market_Tax_Category;
      Receipt    : WL.Money.Money_Type)
   is
      use WL.Money;
      Tithe      : constant Money_Type :=
                     Tax (Receipt, Float (Government.Owner_Tithe));
   begin

      Government.Update.Add_Cash (Receipt - Tithe);
      Government.Update.Tax_Receipts (Category) :=
        Government.Tax_Receipts (Category) + Receipt;
      Government.Owner.Variable_Reference.Add_Cash (Tithe);

      Government.Log_Trade
        ("from sale of "
         & WL.Quantities.Image (Quantity)
         & " "
         & Commodity.Name
         & " @ "
         & WL.Money.Image (Price)
         & ", tax receipt is "
         & WL.Money.Image (Receipt)
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
      return WL.Money.Money_Type
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

end Concorde.Government;
