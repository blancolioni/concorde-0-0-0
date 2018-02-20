with Concorde.Trades;

package Concorde.Powers.Taxation is

   function Set_Tax_Rate
     (Category : Concorde.Trades.Market_Tax_Category)
      return Power_Type;

   function Collect_Tax
     (Category : Concorde.Trades.Market_Tax_Category)
      return Power_Type;

private

   type Taxation_Power is
     abstract new Root_Power_Type with
      record
         Category : Concorde.Trades.Market_Tax_Category;
      end record;

   function Base_Id (Power : Taxation_Power) return String is abstract;

   overriding function Class_Identifier
     (Power : Taxation_Power)
      return String;

end Concorde.Powers.Taxation;
