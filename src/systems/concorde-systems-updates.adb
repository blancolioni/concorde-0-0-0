with Concorde.Empires;
with Concorde.Ships.Create;

package body Concorde.Systems.Updates is

   Base_Loyalty_Change : constant := 0.002;

   -------------------
   -- Update_System --
   -------------------

   procedure Update_System (System : in out Root_Star_System_Type'Class) is
   begin
      if System.Owned then
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
               declare
                  Ship : constant Concorde.Ships.Ship_Type :=
                           Concorde.Ships.Create.New_Ship
                             (Owner  => System.Owner,
                              System => System,
                              Design => "defender");
               begin
                  System.Ships.Append (Ship);
               end;

               System.Progress := System.Progress - 1.0;
            else
               System.Progress := 1.0;
            end if;
         end if;
      end if;
   end Update_System;

end Concorde.Systems.Updates;
