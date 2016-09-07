package Concorde.Updates is

   procedure Start_Updates;
   procedure Stop_Updates;

   procedure Set_Time_Acceleration (Acceleration : Non_Negative_Real);

   procedure Perform_Update
     (Execute_Battles  : Boolean;
      Check_Invariants : Boolean);

   procedure Begin_Render;
   procedure Finish_Render;

   procedure Tick
     (Actual_Seconds : Duration);

end Concorde.Updates;
