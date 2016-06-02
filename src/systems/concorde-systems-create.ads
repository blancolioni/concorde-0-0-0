with WL.Work;

package Concorde.Systems.Create is

   function New_System
     (Index       : Positive;
      Name        : String;
      Work_Handle : WL.Work.Work_Handle;
      X, Y        : Real;
      Boundary    : System_Influence_Boundary;
      Production  : Non_Negative_Real;
      Capacity    : Non_Negative_Real)
      return Star_System_Type;

end Concorde.Systems.Create;
