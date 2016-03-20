with Concorde.Galaxy.Locking;

with Concorde.Ships.Battles;
with Concorde.Ships.Lists;

package body Concorde.Galaxy.Ships is

   -----------------
   -- Can_Move_To --
   -----------------

   function Can_Move_To
     (Ship : Concorde.Ships.Ship_Type;
      Destination : Concorde.Systems.Star_System_Type)
      return Boolean
   is
      function System_OK
        (System : Concorde.Systems.Star_System_Type)
         return Boolean
      is (System.Owner = null or else System.Owner = Ship.Owner);

   begin
      return Concorde.Systems.Graphs.Cost
        (Galaxy_Graph.Shortest_Path
           (Ship.System.Index, Destination.Index, System_OK'Access))
          > 0.0;
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
      P : constant Path :=
            Galaxy_Graph.Shortest_Path
              (Ship.System.Index, Ship.Destination.Index);
   begin
      if Vertex_Count (P) > 1 then
         declare
            use Concorde.Systems;
            From : constant Star_System_Access :=
                     Galaxy_Vector.Element (Ship.System.Index);
            To   : constant Star_System_Access :=
                     Galaxy_Vector.Element (Galaxy_Graph.Next (P).Index);
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

         end;
      else
         Ship.Set_Destination (null);
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
