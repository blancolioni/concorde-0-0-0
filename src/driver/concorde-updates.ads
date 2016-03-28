package Concorde.Updates is

   type Update_Speed is range 0 .. 5;

   procedure Start_Updates;
   procedure Stop_Updates;

   procedure Set_Update_Speed
     (Speed : Update_Speed);

   procedure Perform_Update
     (Execute_Battles : Boolean);

   procedure Begin_Render;
   procedure Finish_Render;

end Concorde.Updates;
