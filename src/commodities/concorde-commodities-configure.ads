with Tropos;

with Concorde.Facilities;

package Concorde.Commodities.Configure is

   procedure Configure_Commodities;

   procedure Configure_Stock
     (From_Config : Tropos.Configuration;
      Stock       : in out Stock_Interface'Class;
      Factor      : Non_Negative_Real := 1.0);

   function Configure_Commodity_Array
     (From_Config : Tropos.Configuration)
      return Array_Of_Commodities;

   procedure Calculate_Base_Prices;

   function New_Pop_Group
     (Identifier : String;
      Base_Price : Concorde.Money.Price_Type)
      return Commodity_Type;

   procedure Configure_Pop_Needs;

end Concorde.Commodities.Configure;
