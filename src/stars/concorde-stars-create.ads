with Concorde.Systems;

package Concorde.Stars.Create is

   function New_Main_Sequence_Star
     (System       : Concorde.Systems.Star_System_Type;
      Name         : String;
      Solar_Masses : Non_Negative_Real)
      return Star_Type;

end Concorde.Stars.Create;
