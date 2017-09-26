package Concorde.Maps.Height_Maps is

   procedure Create_Height_Map
     (Layout    : in out Tile_Layout_Interface'Class;
      Frequency : Frequency_Array;
      Smoothing : Positive);

end Concorde.Maps.Height_Maps;
