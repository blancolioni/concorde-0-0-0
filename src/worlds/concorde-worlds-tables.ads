with Lui.Colours;

private package Concorde.Worlds.Tables is

   function Height_Colour
     (Height : Height_Range)
      return Lui.Colours.Colour_Type;

   function Temperature_Colour
     (Temperature : Non_Negative_Real)
      return Lui.Colours.Colour_Type;

end Concorde.Worlds.Tables;
