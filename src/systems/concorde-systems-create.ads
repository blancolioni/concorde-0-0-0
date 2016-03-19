package Concorde.Systems.Create is

   function New_System
     (Index      : Positive;
      Name       : String;
      X, Y       : Real;
      Production : Non_Negative_Real;
      Capacity   : Non_Negative_Real)
      return Star_System_Access;

end Concorde.Systems.Create;
