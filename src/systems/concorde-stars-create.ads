package Concorde.Stars.Create is

   function New_Main_Sequence_Star
     (Name         : String;
      System       : Concorde.Systems.Star_System_Type;
      Solar_Masses : Non_Negative_Real)
      return Star_Type;

end Concorde.Stars.Create;
