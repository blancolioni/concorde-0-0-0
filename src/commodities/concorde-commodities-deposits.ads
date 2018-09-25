package Concorde.Commodities.Deposits is

   type Normal_Curve_Constraints is
      record
         Mean : Unit_Real;
         Standard_Deviation : Unit_Real;
      end record;

   function Abundance_Constraints
     (Commodity : Commodity_Type)
      return Normal_Curve_Constraints
     with Pre => Commodity.Class = Resource;

   function Concentration_Constraints
     (Commodity : Commodity_Type)
      return Normal_Curve_Constraints
     with Pre => Commodity.Class = Resource;

   procedure Configure_Resource
     (Commodity : Commodity_Type;
      Abundance : Normal_Curve_Constraints;
      Concentration : Normal_Curve_Constraints);

end Concorde.Commodities.Deposits;
