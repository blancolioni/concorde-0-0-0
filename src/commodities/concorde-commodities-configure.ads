with Tropos;

with Concorde.Facilities;

package Concorde.Commodities.Configure is

   procedure Configure_Commodities;

   procedure Configure_Stock
     (From_Config : Tropos.Configuration;
      Stock       : in out Stock_Interface'Class;
      Factor      : Non_Negative_Real := 1.0);

   procedure Calculate_Base_Prices;

   function New_Pop_Group
     (Identifier : String;
      Base_Price : Concorde.Money.Price_Type)
      return Commodity_Type;

end Concorde.Commodities.Configure;
