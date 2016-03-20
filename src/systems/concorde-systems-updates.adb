with Concorde.Empires;
with Concorde.Ships.Create;

package body Concorde.Systems.Updates is

   -------------------
   -- Update_System --
   -------------------

   procedure Update_System (System : Star_System_Access) is
   begin
      if System.Owner /= null then
         System.Progress := System.Progress + System.Production;
         if System.Progress >= 1.0 then
            if System.Owner.Available_Ship_Capacity > 0 then
               Concorde.Ships.Create.New_Ship
                 (Owner  => Concorde.Empires.Empire_Type (System.Owner),
                  System => System,
                  Max_HP => 10);
               System.Progress := System.Progress - 1.0;
            else
               System.Progress := 1.0;
            end if;
         end if;
      end if;
   end Update_System;

end Concorde.Systems.Updates;
