with WL.String_Maps;

package body Concorde.Commodities.Deposits is

   type Deposit_Record is
      record
         Abundance     : Normal_Curve_Constraints;
         Concentration : Normal_Curve_Constraints;
      end record;

   package Deposit_Maps is
     new WL.String_Maps (Deposit_Record);

   Deposit_Map : Deposit_Maps.Map;

   ---------------------------
   -- Abundance_Constraints --
   ---------------------------

   function Abundance_Constraints
     (Commodity : Commodity_Type)
      return Normal_Curve_Constraints
   is
   begin
      return Deposit_Map.Element (Commodity.Identifier).Abundance;
   end Abundance_Constraints;

   -------------------------------
   -- Concentration_Constraints --
   -------------------------------

   function Concentration_Constraints
     (Commodity : Commodity_Type)
      return Normal_Curve_Constraints
   is
   begin
      return Deposit_Map.Element (Commodity.Identifier).Concentration;
   end Concentration_Constraints;

   ------------------------
   -- Configure_Resource --
   ------------------------

   procedure Configure_Resource
     (Commodity : Commodity_Type;
      Abundance : Normal_Curve_Constraints;
      Concentration : Normal_Curve_Constraints)
   is
   begin
      Deposit_Map.Insert (Commodity.Identifier, (Abundance, Concentration));
   end Configure_Resource;

end Concorde.Commodities.Deposits;
