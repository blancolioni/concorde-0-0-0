with WL.Random;

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

      function System_Priority
        (System : Concorde.Systems.Star_System_Type)
         return Natural
      is
         Salt : constant Natural := WL.Random.Random_Number (1, 100);
      begin
         if not Concorde.Galaxy.Ships.Can_Move_To (Ship, System) then
            return Natural'Last;
         elsif System.Owner = Ship.Owner then
            return System.Ships * 100 + Salt;
         else
            return Salt;
         end if;
      end System_Priority;

   begin

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

      if Ship.Destination /= null then
         Concorde.Empires.Logging.Log
           (Ship.Owner,
            "warship " & Ship.Name
            & " at " & Ship.System.Name
            & " on its way to "
            & Ship.Destination.Name);

         Concorde.Galaxy.Ships.Move_Ship (Ship);
      elsif Ship.HP < Ship.Max_HP then
         Ship.HP := Ship.HP + 1;
      end if;
   end Update_Ship;

   ------------------
   -- Update_Ships --
   ------------------

   procedure Update_Ship_Movement is
   begin
      for Ship of Ship_Vector loop
         if Ship.HP > 0 then
            Update_Ship (Ship);
         end if;
      end loop;
   end Update_Ship_Movement;

end Concorde.Ships.Updates;
