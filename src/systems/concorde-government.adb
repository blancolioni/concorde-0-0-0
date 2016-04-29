with Concorde.Government.Db;

package body Concorde.Government is

   ----------------------
   -- Add_Trade_Offers --
   ----------------------

   overriding procedure Add_Trade_Offers
     (Item   : not null access constant Root_Government_Type;
      Market : in out Concorde.Trades.Trade_Interface'Class)
   is
      pragma Unreferenced (Item);
      pragma Unreferenced (Market);
   begin
      null;
   end Add_Trade_Offers;

   -----------------------
   -- Basic_Living_Wage --
   -----------------------

   function Basic_Living_Wage
     (Government : Root_Government_Type'Class)
      return Boolean
   is
   begin
      return Government.Basic_Living_Wage;
   end Basic_Living_Wage;

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

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Government : Root_Government_Type)
      return Memor.Root_Database_Type'Class
   is
      pragma Unreferenced (Government);
   begin
      return Db.Get_Database;
   end Object_Database;

   ---------------------
   -- On_Update_Start --
   ---------------------

   overriding procedure On_Update_Start
     (Government : in out Root_Government_Type)
   is
   begin
      Government.Tax_Receipts := (others => Concorde.Money.Zero);
   end On_Update_Start;

   --------------
   -- Tax_Rate --
   --------------

   overriding function Tax_Rate
     (Government : Root_Government_Type;
      Category   : Concorde.Trades.Market_Tax_Category;
      Commodity  : Concorde.Commodities.Commodity_Type)
      return Unit_Real
   is
   begin
      return Government.Tax_Rates.Element (Commodity.Reference) (Category);
   end Tax_Rate;

   -----------------
   -- Tax_Receipt --
   -----------------

   overriding procedure Tax_Receipt
     (Government : in out Root_Government_Type;
      Commodity  : Concorde.Commodities.Commodity_Type;
      Quantity   : Concorde.Quantities.Quantity;
      Price      : Concorde.Money.Price_Type;
      Category   : Concorde.Trades.Market_Tax_Category;
      Receipt    : Concorde.Money.Money_Type)
   is
      use type Concorde.Money.Money_Type;
   begin
      Government.Log_Trade
        ("from sale of "
         & Concorde.Quantities.Image (Quantity)
         & " "
         & Commodity.Name
         & " @ "
         & Concorde.Money.Image (Price)
         & ", tax receipt is "
         & Concorde.Money.Image (Receipt));
      Government.Add_Cash (Receipt);
      Government.Tax_Receipts (Category) :=
        Government.Tax_Receipts (Category) + Receipt;
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

end Concorde.Government;
