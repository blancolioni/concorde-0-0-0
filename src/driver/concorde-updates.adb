with Concorde.Dates;
with Concorde.Empires.Updates;
with Concorde.Galaxy.Updates;

package body Concorde.Updates is

   task Update_Task is
      entry Start;
      entry Stop;
   end Update_Task;

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
      Update_Task.Start;
   end Start_Updates;

   ------------------
   -- Stop_Updates --
   ------------------

   procedure Stop_Updates is
   begin
      Update_Task.Stop;
   end Stop_Updates;

   -----------------
   -- Update_Task --
   -----------------

   task body Update_Task is
   begin
      select
         accept Start;
      or
         terminate;
      end select;

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
