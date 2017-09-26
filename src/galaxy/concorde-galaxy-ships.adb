with Concorde.Galaxy.Locking;

with Concorde.Factions;
with Concorde.Factions.Logging;

with Concorde.Ships.Battles;
with Concorde.Ships.Lists;

package body Concorde.Galaxy.Ships is

   -----------------
   -- Can_Move_To --
   -----------------

   function Can_Move_To
     (Ship : Concorde.Ships.Root_Ship_Type'Class;
      Destination : Concorde.Systems.Star_System_Type)
      return Boolean
   is
      use type Concorde.Systems.Star_System_Type;
   begin
      return Destination = Ship.Current_System
        or else Neighbours (Ship.Current_System, Destination)
        or else Ship.Owner.Next_Path_Node_Index
          (Ship.Current_System, Destination) > 0;
   end Can_Move_To;

   -----------------------
   -- Commit_Ship_Moves --
   -----------------------

   procedure Commit_Ship_Moves is

      procedure Update_System
        (System : Concorde.Systems.Star_System_Type);

      -------------------
      -- Update_System --
      -------------------

      procedure Update_System
        (System : Concorde.Systems.Star_System_Type)
      is
      begin
         System.Update.Commit_Ship_Movement;
         declare
            Ship_List : Concorde.Ships.Lists.List;
         begin
            System.Get_Ships (Ship_List);
            if Concorde.Ships.Battles.Has_Conflict (Ship_List) then
               declare
                  use Combat.Ship_Combat;
                  Arena : constant Combat.Ship_Combat.Space_Combat_Arena :=
                            Concorde.Ships.Battles.Create_Arena
                              (System, Ship_List);
               begin
                  if Arena /= null then
                     Concorde.Galaxy.Add_Battle (Arena);
                  end if;
               end;
            end if;
         end;
      end Update_System;

   begin
      Concorde.Systems.Scan_Systems (Update_System'Access);
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
                       (Ship.Current_System, Ship.Destination.System);
      From : constant Star_System_Type := Ship.Current_System;
      To         : constant Star_System_Type :=
                     (if Next_Index = 0
                      then null
                      else Galaxy_Graph.Vertex (Next_Index));
   begin

      if Next_Index = 0 then
         Concorde.Factions.Logging.Log
           (Ship.Owner,
            Ship.Short_Description &
            ": movement canceled because path from "
            & Ship.Current_System.Name
            & " to "
            & Ship.Destination.Name
            & " is blocked");
         Ship.Update.Clear_Destination;
         return;
      end if;

      if From.Index < To.Index then
         Locking.Lock_System (From, True);
         Locking.Lock_System (To, True);
      else
         Locking.Lock_System (To, True);
         Locking.Lock_System (From, True);
      end if;

      From.Update.Departing (Ship);
      From.Update.Add_Traffic (To);
      To.Update.Arriving (Ship);

      if From.Index < To.Index then
         Locking.Unlock_System (To);
         Locking.Unlock_System (From);
      else
         Locking.Unlock_System (From);
         Locking.Unlock_System (To);
      end if;

   end Move_Ship;

   ----------------------
   -- Start_Ship_Moves --
   ----------------------

   procedure Start_Ship_Moves is

      procedure Update
        (System : Concorde.Systems.Star_System_Type);

      ------------
      -- Update --
      ------------

      procedure Update
        (System : Concorde.Systems.Star_System_Type)
      is
      begin
         Concorde.Galaxy.Locking.Lock_System (System, True);
         System.Update.Clear_Ship_Movement;
         Concorde.Galaxy.Locking.Unlock_System (System);
      end Update;

   begin
      Concorde.Systems.Scan_Systems (Update'Access);
   end Start_Ship_Moves;

end Concorde.Galaxy.Ships;
