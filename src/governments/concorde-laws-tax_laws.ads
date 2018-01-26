with Concorde.Commodities;

package Concorde.Laws.Tax_Laws is

   type Root_Tax_Law_Type is
     abstract new Root_Law_Type with private;

   type Tax_Law_Type is access constant Root_Tax_Law_Type'Class;

   function Rate (Law : Root_Tax_Law_Type'Class) return Unit_Real;

   function Sales_Tax
     (Context   : Law_Context;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      Rate      : Unit_Real)
      return Law_Type;

   function Import_Tariff
     (Context   : Law_Context;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      Rate      : Unit_Real)
      return Law_Type;

   function Sales_Tax
     (Context   : Law_Context;
      Rate      : Unit_Real)
      return Law_Type;

   function Import_Tariff
     (Context   : Law_Context;
      Rate      : Unit_Real)
      return Law_Type;

private

   type Root_Tax_Law_Type is
     abstract new Root_Law_Type with
      record
         Previous_Rate : Unit_Real;
         New_Rate      : Unit_Real;
      end record;

end Concorde.Laws.Tax_Laws;
