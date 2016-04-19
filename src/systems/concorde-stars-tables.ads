private package Concorde.Stars.Tables is

   procedure Get_Main_Sequence_Info
     (Solar_Masses : Non_Negative_Real;
      Class        : out Stellar_Class_Type;
      Subclass     : out Stellar_Subclass_Type;
      Radius       : out Non_Negative_Real;
      Luminosity   : out Non_Negative_Real;
      Colour       : out Lui.Colours.Colour_Type);

end Concorde.Stars.Tables;
