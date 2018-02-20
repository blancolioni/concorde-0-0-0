package body Concorde.Powers.Taxation is

   overriding function Class_Identifier
     (Power : Taxation_Power)
      return String
   is (Taxation_Power'Class (Power).Base_Id
       & (case Power.Category is
             when Concorde.Trades.Sales => "_sales_tax",
             when Concorde.Trades.Import => "_import_tariffs",
             when Concorde.Trades.Export => "_export_tariffs"));

   type Collect_Tax_Power is new Taxation_Power with null record;

   overriding function Base_Id (Power : Collect_Tax_Power) return String
   is ("collect");

   type Set_Tax_Rate_Power is new Taxation_Power with null record;

   overriding function Base_Id (Power : Set_Tax_Rate_Power) return String
   is ("set");

   -----------------
   -- Collect_Tax --
   -----------------

   function Collect_Tax
     (Category : Concorde.Trades.Market_Tax_Category)
      return Power_Type
   is
   begin
      return Collect_Tax_Power'
        (Category => Category);
   end Collect_Tax;

   ------------------
   -- Set_Tax_Rate --
   ------------------

   function Set_Tax_Rate
     (Category : Concorde.Trades.Market_Tax_Category)
      return Power_Type
   is
   begin
      return Set_Tax_Rate_Power'
        (Category => Category);
   end Set_Tax_Rate;

end Concorde.Powers.Taxation;
