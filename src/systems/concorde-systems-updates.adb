with Concorde.Empires;

package body Concorde.Systems.Updates is

   -------------------
   -- Update_System --
   -------------------

   procedure Update_System (System : in out Root_Star_System_Type'Class) is
   begin
      if System.Owner /= null then
         System.Progress := System.Progress + System.Production;
         if System.Progress >= 1.0 then
            if System.Owner.Available_Fleet_Capacity > 0 then
               if Real (System.Fleets) < System.Capacity then
                  System.Fleets := System.Fleets + 1;
                  System.Owner.New_Fleets (1);
               end if;
               System.Progress := System.Progress - 1.0;
            else
               System.Progress := 1.0;
            end if;
         end if;
      end if;
   end Update_System;

end Concorde.Systems.Updates;
