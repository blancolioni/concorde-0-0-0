with Concorde.Systems.Updates;

package body Concorde.Galaxy.Updates is

   -------------------
   -- Update_Galaxy --
   -------------------

   procedure Update_Galaxy is
   begin
      for System of Galaxy_Vector loop
         Concorde.Systems.Updates.Update_System (System.all);
      end loop;
   end Update_Galaxy;

end Concorde.Galaxy.Updates;
