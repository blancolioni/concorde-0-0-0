with Concorde.Stars;

package Concorde.Systems.Create is

   function New_System
     (Index       : Positive;
      Name        : String;
      X, Y, Z     : Real;
      Primary     : Concorde.Stars.Star_Type := null)
      return Star_System_Type;

end Concorde.Systems.Create;
