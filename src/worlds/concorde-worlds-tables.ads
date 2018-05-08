with Lui.Colors;

private package Concorde.Worlds.Tables is

   function Height_Color
     (Height : Height_Range)
      return Lui.Colors.Color_Type;

   function Temperature_Color
     (Temperature : Non_Negative_Real)
      return Lui.Colors.Color_Type;

end Concorde.Worlds.Tables;
