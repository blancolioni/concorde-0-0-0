private package Concorde.Generate.Surfaces.Heights is

   procedure Create_Height_Array
     (Heights           : out Surface_Array;
      Smoothing         :     Natural;
      Min, Max          : Integer;
      Random            : Boolean;
      Initial_Heights   : access
        function (X, Y : Positive) return Real);

end Concorde.Generate.Surfaces.Heights;
