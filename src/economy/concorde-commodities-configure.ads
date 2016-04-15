with Concorde.Facilities;

package Concorde.Commodities.Configure is

   procedure Configure_Commodities;

   function Create_From_Skill
     (Tag      : String;
      Name     : String;
      Base_Pay : Concorde.Money.Price_Type)
      return Commodity_Type;

   procedure Create_From_Service
     (Service_Facility : Concorde.Facilities.Facility_Type);

   procedure Calculate_Base_Prices;

end Concorde.Commodities.Configure;
