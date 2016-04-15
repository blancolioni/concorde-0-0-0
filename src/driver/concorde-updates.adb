with Ada.Unchecked_Deallocation;

with Concorde.Dates;

with Concorde.Economy.Updates;
with Concorde.Empires.Updates;
with Concorde.Galaxy.Updates;
with Concorde.Ships.Updates;

with Concorde.Ships.Invariants;

with Concorde.Galaxy.Locking;

with Concorde.Locking;
with Concorde.Options;

with Concorde.Empires.Logging;

package body Concorde.Updates is

   Update_Delay : constant array (Update_Speed) of Duration :=
                    (0.1, 2.0, 1.0, 0.5, 0.2, 0.01);

   task type Update_Task is
      entry Stop;
      entry Set_Speed (New_Speed : Update_Speed);
   end Update_Task;

   type Update_Task_Access is access Update_Task;

   procedure Free is
     new Ada.Unchecked_Deallocation (Update_Task, Update_Task_Access);

   Current_Update_Task : Update_Task_Access;

   Render_Lock : Concorde.Locking.Lock;

   Check_Invariants   : Boolean := False;
   Show_Battle_Screen : Boolean := True;

   ------------------
   -- Begin_Render --
   ------------------

   procedure Begin_Render is
   begin
      Render_Lock.Exclusive;
   end Begin_Render;

   -------------------
   -- Finish_Render --
   -------------------

   procedure Finish_Render is
   begin
      Render_Lock.Unlock;
   end Finish_Render;

   --------------------
   -- Perform_Update --
   --------------------

   procedure Perform_Update
     (Execute_Battles  : Boolean;
      Check_Invariants : Boolean)
   is
   begin
      Render_Lock.Exclusive;
      Concorde.Dates.Tick;

      if Check_Invariants then
         Concorde.Empires.Check_Invariants;
         Concorde.Ships.Invariants.Check_Invariants;
      end if;

      Concorde.Ships.Updates.Delete_Dead_Ships;

      Concorde.Empires.Updates.Update_Empires;
      Concorde.Galaxy.Updates.Update_Galaxy;

      Concorde.Economy.Updates.Daily_Update;

      if Execute_Battles then
         Concorde.Galaxy.Complete_Battles;
      end if;

      Concorde.Empires.Logging.Flush_Log;

      if Check_Invariants then
         Concorde.Empires.Check_Invariants;
         Concorde.Ships.Invariants.Check_Invariants;
      end if;

      Render_Lock.Unlock;
   end Perform_Update;

   ----------------------
   -- Set_Update_Speed --
   ----------------------

   procedure Set_Update_Speed
     (Speed : Update_Speed)
   is
   begin
      Concorde.Galaxy.Complete_Battles;
      Current_Update_Task.Set_Speed (Speed);
   end Set_Update_Speed;

   -------------------
   -- Start_Updates --
   -------------------

   procedure Start_Updates is
   begin
      Check_Invariants := Concorde.Options.Check_Invariants;
      Show_Battle_Screen := Concorde.Options.Show_Battle_Screen;
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
      Speed : Update_Speed := 0;
   begin
      loop
         select
            accept Stop;
            exit;
         or
            accept Set_Speed (New_Speed : in Update_Speed) do
               Speed := New_Speed;
            end Set_Speed;
         else
            delay Update_Delay (Speed);
            if Speed > 0 then
               Perform_Update
                 (Execute_Battles  => not Show_Battle_Screen,
                  Check_Invariants => Check_Invariants);

               if Concorde.Galaxy.Battle_Count > 0
                 and then Show_Battle_Screen
               then
                  Speed := 0;
               end if;
            end if;
         end select;
      end loop;
   end Update_Task;

end Concorde.Updates;
