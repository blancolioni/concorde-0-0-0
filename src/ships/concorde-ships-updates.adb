with WL.Random;

with Concorde.AI;
with Concorde.Galaxy.Ships;
with Concorde.Empires;
with Concorde.Systems;

with Concorde.Empires.Logging;

package body Concorde.Ships.Updates is

   procedure Update_Ship (Ship : Ship_Type);

   -----------------
   -- Update_Ship --
   -----------------

   procedure Update_Ship (Ship : Ship_Type) is

      function System_Priority
        (System : Concorde.Systems.Star_System_Type)
         return Natural;

      ---------------------
      -- System_Priority --
      ---------------------

      function System_Priority
        (System : Concorde.Systems.Star_System_Type)
         return Natural
      is
         Salt : constant Natural := WL.Random.Random_Number (1, 100);
      begin
         if not Concorde.Galaxy.Ships.Can_Move_To (Ship, System) then
            return Natural'Last;
         elsif System.Owner = Ship.Owner then
            return System.Ships * 10
              + Ship.Owner.Path_Length (Ship.System, System)
              + Salt;
         else
            return Salt + Ship.Owner.Path_Length (Ship.System, System);
         end if;
      end System_Priority;

   begin

      if False then
         for N of Concorde.Galaxy.Neighbours (Ship.System) loop
            if Ship.Owner.Has_Focus (N)
              and then N.Owner /= null
              and then N.Owner /= Ship.Owner
            then
               Ship.Destination := N;
               exit;
            end if;
         end loop;

         if Ship.Destination = null
           and then not Ship.Owner.Has_Focus (Ship.System)
         then
            Ship.Destination :=
              Ship.Owner.Minimum_Score_Focus
                (System_Priority'Access);
         end if;

         if Ship.Destination = Ship.System then
            Ship.Destination := null;
         end if;
      end if;

--        if Ship.HP > 0
--       and then Ship.Owner.Maximum_Supported_Ships < Ship.Owner.Current_Ships
--       and then WL.Random.Random_Number (1, 100)
--       > Ship.Owner.Maximum_Supported_Ships * 100 / Ship.Owner.Current_Ships
--        then
--           Ship.HP := Ship.HP - 1;
--        end if;

      Ship.Owner.AI.Order_Ship (Ship);

      if Ship.Destination /= null then
         Concorde.Empires.Logging.Log
           (Ship.Owner,
            Ship.Name
            & " at " & Ship.System.Name
            & " on its way to "
            & Ship.Destination.Name
            & " (distance"
            & Natural'Image
              (Ship.Owner.Path_Length (Ship.System, Ship.Destination))
            & ")");

         Concorde.Galaxy.Ships.Move_Ship (Ship);
--        elsif Ship.HP < Ship.Max_HP
--          and then Ship.Owner.Available_Ship_Capacity >= 0
--        then
--           Ship.HP := Ship.HP + 1;
      end if;
   end Update_Ship;

   ------------------
   -- Update_Ships --
   ------------------

   procedure Update_Ship_Movement is
   begin
      for Ship of Ship_Vector loop
         if Ship.Damage < 1.0 then
            Update_Ship (Ship);
         end if;
      end loop;
   end Update_Ship_Movement;

end Concorde.Ships.Updates;
