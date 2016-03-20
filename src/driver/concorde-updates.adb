with Ada.Unchecked_Deallocation;

with Concorde.Dates;
with Concorde.Empires.Updates;
with Concorde.Galaxy.Updates;

with Concorde.Galaxy.Locking;

package body Concorde.Updates is

   task type Update_Task is
      entry Stop;
   end Update_Task;

   type Update_Task_Access is access Update_Task;

   procedure Free is
     new Ada.Unchecked_Deallocation (Update_Task, Update_Task_Access);

   Current_Update_Task : Update_Task_Access;

   --------------------
   -- Perform_Update --
   --------------------

   procedure Perform_Update is
   begin
      Concorde.Dates.Tick;
      Concorde.Galaxy.Updates.Update_Galaxy;
      Concorde.Empires.Updates.Update_Empires;
   end Perform_Update;

   -------------------
   -- Start_Updates --
   -------------------

   procedure Start_Updates is
   begin
      Concorde.Galaxy.Locking.Init_Locking;
      Current_Update_Task := new Update_Task;
   end Start_Updates;

   ------------------
   -- Stop_Updates --
   ------------------

   procedure Stop_Updates is
   begin
      Current_Update_Task.Stop;
      Free (Current_Update_Task);
   end Stop_Updates;

   -----------------
   -- Update_Task --
   -----------------

   task body Update_Task is
   begin
      loop
         select
            accept Stop;
            exit;
         else
            delay 0.2;
            Concorde.Dates.Tick;
            Concorde.Galaxy.Updates.Update_Galaxy;
            Concorde.Empires.Updates.Update_Empires;
         end select;
      end loop;
   end Update_Task;

end Concorde.Updates;
