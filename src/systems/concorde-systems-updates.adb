with Concorde.Empires;
with Concorde.Ships.Create;

package body Concorde.Systems.Updates is

   Base_Loyalty_Change : constant := 0.002;

   -------------------
   -- Update_System --
   -------------------

   procedure Update_System (System : Star_System_Access) is
   begin
      if System.Owner /= null then
         if System.Loyalty < 1.0 then
            if 1.0 - System.Loyalty <= Base_Loyalty_Change then
               System.Loyalty := 1.0;
               System.Original_Owner := null;
            else
               System.Loyalty := System.Loyalty + Base_Loyalty_Change;
            end if;
         end if;

         System.Progress := System.Progress
           + System.Production * System.Loyalty;
         if System.Progress >= 1.0 then
            if System.Owner.Available_Ship_Capacity > 0 then
               Concorde.Ships.Create.New_Ship
                 (Owner  => Concorde.Empires.Empire_Type (System.Owner),
                  System => System,
                  Design => "defender");
               System.Progress := System.Progress - 1.0;
            else
               System.Progress := 1.0;
            end if;
         end if;
      end if;
   end Update_System;

end Concorde.Systems.Updates;
