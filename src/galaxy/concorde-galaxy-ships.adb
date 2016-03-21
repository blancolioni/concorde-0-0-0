with Concorde.Galaxy.Locking;

with Concorde.Empires;

with Concorde.Ships.Battles;
with Concorde.Ships.Lists;

with Concorde.AI;

package body Concorde.Galaxy.Ships is

   -----------------
   -- Can_Move_To --
   -----------------

   function Can_Move_To
     (Ship : Concorde.Ships.Ship_Type;
      Destination : Concorde.Systems.Star_System_Type)
      return Boolean
   is
      use type Concorde.Systems.Star_System_Type;
   begin
      return Destination = Ship.System
        or else Neighbours (Ship.System, Destination)
        or else Ship.Owner.Next_Path_Node_Index (Ship.System, Destination) > 0;
   end Can_Move_To;

   -----------------------
   -- Commit_Ship_Moves --
   -----------------------

   procedure Commit_Ship_Moves is
   begin
      for System of Galaxy_Vector loop
         System.Commit_Ship_Movement;
         declare
            Ship_List : Concorde.Ships.Lists.List;
         begin
            System.Get_Ships (Ship_List);
            if Concorde.Ships.Battles.Has_Conflict (Ship_List) then
               Concorde.Ships.Battles.Fight (System, Ship_List);
            end if;
         end;
      end loop;
   end Commit_Ship_Moves;

   ---------------
   -- Move_Ship --
   ---------------

   procedure Move_Ship
     (Ship : Concorde.Ships.Ship_Type)
   is
      use Concorde.Systems.Graphs;
      use Concorde.Systems;

      Next_Index : constant Natural :=
                     Ship.Owner.Next_Path_Node_Index
                       (Ship.System, Ship.Destination);
      From : constant Star_System_Access :=
               Galaxy_Vector.Element (Ship.System.Index);
      To   : constant Star_System_Access :=
               Galaxy_Vector.Element (Next_Index);
   begin

      if From.Index < To.Index then
         Locking.Lock_System (From, True);
         Locking.Lock_System (To, True);
      else
         Locking.Lock_System (To, True);
         Locking.Lock_System (From, True);
      end if;

      From.Departing (Ship);
      To.Arriving (Ship);
      From.Add_Traffic (To, 1);

      if From.Index < To.Index then
         Locking.Unlock_System (To);
         Locking.Unlock_System (From);
      else
         Locking.Unlock_System (From);
         Locking.Unlock_System (To);
      end if;

      if To = Ship.Destination then
         Ship.Owner.AI.Wake;
      end if;

   end Move_Ship;

   ----------------------
   -- Start_Ship_Moves --
   ----------------------

   procedure Start_Ship_Moves is
   begin
      for System of Galaxy_Vector loop
         Concorde.Galaxy.Locking.Lock_System (System, True);
         System.Clear_Ship_Movement;
         Concorde.Galaxy.Locking.Unlock_System (System);
      end loop;
   end Start_Ship_Moves;

end Concorde.Galaxy.Ships;
