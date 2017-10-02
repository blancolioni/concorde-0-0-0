with Ada.Unchecked_Deallocation;

with Concorde.Dates;
--  with Concorde.Logging;

with Concorde.Objects.Queues;

--  with Concorde.Agents.Updates;
--  with Concorde.Economy.Updates;
--  with Concorde.Factions.Updates;
--
--  with Concorde.Ships.Invariants;

with Concorde.Galaxy.Locking;

with Concorde.Locking;

--  with Concorde.Factions.Logging;

package body Concorde.Updates is

--     Update_Delay : constant array (Update_Speed) of Duration :=
--                      (0.1, 2.0, 1.0, 0.5, 0.2, 0.01);

   task type Update_Task is
      entry Update;
      entry Stop;
   end Update_Task;

   type Update_Task_Access is access Update_Task;

   task Date_Manager_Task is
      entry Set_Speed_Factor (Factor : Non_Negative_Real);
      entry Tick (Actual_Seconds : Duration);
   end Date_Manager_Task;

   procedure Free is
     new Ada.Unchecked_Deallocation (Update_Task, Update_Task_Access);

   Current_Update_Task : Update_Task_Access;

   Render_Lock : Concorde.Locking.Lock;

   -------------
   -- Advance --
   -------------

   procedure Advance (Interval : Duration) is
   begin
      Render_Lock.Exclusive;
      Concorde.Dates.Tick (Interval);
      Concorde.Objects.Queues.Scan_Queue;
      Render_Lock.Unlock;
   end Advance;

   ------------------
   -- Begin_Render --
   ------------------

   procedure Begin_Render is
   begin
      Render_Lock.Exclusive;
   end Begin_Render;

   -----------------------
   -- Date_Manager_Task --
   -----------------------

   task body Date_Manager_Task is
      Current_Acceleration : Non_Negative_Real := 0.0;
   begin
      loop
         select
            accept Tick (Actual_Seconds : in Duration) do
               Concorde.Dates.Tick
                 (Actual_Seconds * Duration (Current_Acceleration));
            end Tick;

            Current_Update_Task.Update;

         or
            accept Set_Speed_Factor (Factor : in Non_Negative_Real) do
               Current_Acceleration := Factor;
            end Set_Speed_Factor;
         or
            terminate;
         end select;
      end loop;

   end Date_Manager_Task;

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

--     procedure Perform_Update
--       (Execute_Battles  : Boolean;
--        Check_Invariants : Boolean)
--     is
--     begin
--        Render_Lock.Exclusive;
--
--        Concorde.Logging.Start_Update;
--
--        if Check_Invariants then
--           Concorde.Factions.Check_Invariants;
--           Concorde.Ships.Invariants.Check_Invariants;
--        end if;
--
--  --      Concorde.Ships.Updates.Delete_Dead_Ships;
--
--        Concorde.Agents.Updates.Update_Agents
--          (Concorde.Agents.Updates.Start_Of_Update'Access);
--
--        Concorde.Objects.Queues.Scan_Queue;
--
--        Concorde.Factions.Updates.Update_Factions;
--  --        Concorde.Galaxy.Updates.Update_Galaxy;
--
--        if False then
--           Concorde.Economy.Updates.Daily_Update;
--        end if;
--
--  --      Concorde.Ships.Updates.Update_Ship_Orders;
--
--        if Execute_Battles then
--           Concorde.Galaxy.Complete_Battles;
--        end if;
--
--        Concorde.Agents.Updates.Update_Agents
--          (Concorde.Agents.Updates.End_Of_Update'Access);
--
--        Concorde.Factions.Logging.Flush_Log;
--
--        if Check_Invariants then
--           Concorde.Factions.Check_Invariants;
--           Concorde.Ships.Invariants.Check_Invariants;
--        end if;
--
--        Concorde.Logging.Finish_Update;
--
--        Render_Lock.Unlock;
--     end Perform_Update;

   ---------------------------
   -- Set_Time_Acceleration --
   ---------------------------

   procedure Set_Time_Acceleration (Acceleration : Non_Negative_Real) is
   begin
      Date_Manager_Task.Set_Speed_Factor (Acceleration);
   end Set_Time_Acceleration;

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

   ----------
   -- Tick --
   ----------

   procedure Tick
     (Actual_Seconds : Duration)
   is
   begin
      Date_Manager_Task.Tick (Actual_Seconds);
   end Tick;

   -----------------
   -- Update_Task --
   -----------------

   task body Update_Task is
   begin
      loop
         select
            accept Stop;
            exit;
         or
            accept Update;
            Concorde.Objects.Queues.Scan_Queue;
         or
            terminate;
         end select;
      end loop;
   end Update_Task;

end Concorde.Updates;