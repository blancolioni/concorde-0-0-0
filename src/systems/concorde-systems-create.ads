package Concorde.Systems.Create is

   function New_System
     (Index       : Positive;
      Name        : String;
      X, Y, Z     : Real)
      return Star_System_Type;

end Concorde.Systems.Create;
