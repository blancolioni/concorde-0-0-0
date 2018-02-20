with Tropos;

with Concorde.Facilities;

package Concorde.Commodities.Configure is

   procedure Configure_Commodities;

   procedure Configure_Stock
     (From_Config : Tropos.Configuration;
      Stock       : in out Stock_Interface'Class;
      Factor      : Non_Negative_Real := 1.0);

   function Create_From_Group
     (Tag      : String;
      Base_Pay : WL.Money.Price_Type)
      return Commodity_Type;

   procedure Create_From_Service
     (Service_Facility : Concorde.Facilities.Facility_Type);

   procedure Calculate_Base_Prices;

end Concorde.Commodities.Configure;
