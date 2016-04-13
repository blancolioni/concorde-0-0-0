with Concorde.Galaxy.Locking;

with Concorde.Empires;
with Concorde.Empires.Logging;

with Concorde.Ships.Battles;
with Concorde.Ships.Lists;

with Concorde.Ships.Db;
with Concorde.Systems.Db;

with Concorde.Players;

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
      return Destination = Ship.System
        or else Neighbours (Ship.System, Destination)
        or else Ship.Owner.Next_Path_Node_Index (Ship.System, Destination) > 0;
   end Can_Move_To;

   -----------------------
   -- Commit_Ship_Moves --
   -----------------------

   procedure Commit_Ship_Moves is

      procedure Update_System
        (System : in out Concorde.Systems.Root_Star_System_Type'Class);

      -------------------
      -- Update_System --
      -------------------

      procedure Update_System
        (System : in out Concorde.Systems.Root_Star_System_Type'Class)
      is
      begin
         System.Commit_Ship_Movement;
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
      Concorde.Systems.Db.Iterate (Update_System'Access);
   end Commit_Ship_Moves;

   ---------------
   -- Move_Ship --
   ---------------

   procedure Move_Ship
     (Ship : in out Concorde.Ships.Root_Ship_Type'Class)
   is
      use Concorde.Systems.Graphs;
      use Concorde.Systems;

      Next_Index : constant Natural :=
                     Ship.Owner.Next_Path_Node_Index
                       (Ship.System, Ship.Destination);
      From : constant Star_System_Type := Ship.System;
      To         : constant Star_System_Type :=
                     (if Next_Index = 0
                      then null
                      else Galaxy_Graph.Vertex (Next_Index));
   begin

      if Next_Index = 0 then
         Concorde.Empires.Logging.Log
           (Ship.Owner,
            Ship.Short_Description &
            ": movement canceled because path from "
            & Ship.System.Name
            & " to "
            & Ship.Destination.Name
            & " is blocked");
         Ship.Clear_Destination;
         return;
      end if;

      if From.Index < To.Index then
         Locking.Lock_System (From.all, True);
         Locking.Lock_System (To.all, True);
      else
         Locking.Lock_System (To.all, True);
         Locking.Lock_System (From.all, True);
      end if;

      declare
         Reference : constant Concorde.Ships.Ship_Type :=
                       Concorde.Ships.Db.Reference (Ship);

         procedure Arrive
           (System : in out Concorde.Systems.Root_Star_System_Type'Class);
         procedure Depart
           (System : in out Concorde.Systems.Root_Star_System_Type'Class);

         ------------
         -- Arrive --
         ------------

         procedure Arrive
           (System : in out Concorde.Systems.Root_Star_System_Type'Class)
         is
         begin
            System.Arriving (Reference);
         end Arrive;

         ------------
         -- Depart --
         ------------

         procedure Depart
           (System : in out Concorde.Systems.Root_Star_System_Type'Class)
         is
         begin
            System.Departing (Reference);
            System.Add_Traffic (To);
         end Depart;

      begin
         Concorde.Systems.Db.Update (From.Reference, Depart'Access);
         Concorde.Systems.Db.Update (To.Reference, Arrive'Access);
      end;

      if From.Index < To.Index then
         Locking.Unlock_System (To.all);
         Locking.Unlock_System (From.all);
      else
         Locking.Unlock_System (From.all);
         Locking.Unlock_System (To.all);
      end if;

      if To = Ship.Destination then
         Ship.Owner.Player.On_Ship_Arrived
           (Ship.Owner, Concorde.Ships.Db.Reference (Ship));
      end if;

   end Move_Ship;

   ----------------------
   -- Start_Ship_Moves --
   ----------------------

   procedure Start_Ship_Moves is

      procedure Update
        (System : in out Concorde.Systems.Root_Star_System_Type'Class);

      ------------
      -- Update --
      ------------

      procedure Update
        (System : in out Concorde.Systems.Root_Star_System_Type'Class)
      is
      begin
         Concorde.Galaxy.Locking.Lock_System (System, True);
         System.Clear_Ship_Movement;
         Concorde.Galaxy.Locking.Unlock_System (System);
      end Update;

   begin
      Concorde.Systems.Db.Iterate (Update'Access);
   end Start_Ship_Moves;

end Concorde.Galaxy.Ships;
