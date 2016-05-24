with Concorde.Galaxy;

with Concorde.Empires.Db;
with Concorde.Systems.Db;

with Concorde.Ships.Lists;
with Concorde.Systems.Lists;

with Concorde.Empires.Logging;

package body Concorde.Players.Simple_Player is

   type Root_Simple_Player_Type is new Root_Player_Type with
      record
         Idle_Ship_List     : Concorde.Ships.Lists.List;
         Owned_Systems      : Concorde.Systems.Lists.List;
         Unexplored_Targets : Concorde.Systems.Lists.List;
      end record;

   overriding procedure On_Start
     (Player : in out Root_Simple_Player_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class);

   overriding procedure On_Ship_Completed
     (Player : in out Root_Simple_Player_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class;
      Ship   : Concorde.Ships.Ship_Type);

   overriding procedure On_Ship_Arrived
     (Player : in out Root_Simple_Player_Type;
      Empire : not null access constant
        Concorde.Empires.Root_Empire_Type'Class;
      Ship   : Concorde.Ships.Ship_Type);

   overriding procedure On_System_Colonised
     (Player : in out Root_Simple_Player_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class;
      System : Concorde.Systems.Root_Star_System_Type'Class;
      Ship   : not null access constant
        Concorde.Ships.Root_Ship_Type'Class);

   procedure Check_Idle_Ships
     (Player : in out Root_Simple_Player_Type'Class;
      Empire : Concorde.Empires.Root_Empire_Type'Class);

   procedure Idle_Ship
     (Player : in out Root_Simple_Player_Type'Class;
      Ship   : not null access constant
        Concorde.Ships.Root_Ship_Type'Class)
       with Pre => not Ship.Has_Orders;

   ----------------------
   -- Check_Idle_Ships --
   ----------------------

   procedure Check_Idle_Ships
     (Player : in out Root_Simple_Player_Type'Class;
      Empire : Concorde.Empires.Root_Empire_Type'Class)
   is
   begin
      while not Player.Idle_Ship_List.Is_Empty
        and then not Player.Unexplored_Targets.Is_Empty
      loop
         declare
            use Concorde.Ships.Lists;
            Closest_Ship : Cursor := No_Element;
            Least_Steps  : Natural := Natural'Last;
            Destination  : constant Concorde.Systems.Star_System_Type :=
                             Player.Unexplored_Targets.First_Element;
            Skip         : Boolean := False;
         begin
            if Destination.Owned then
               Concorde.Empires.Logging.Log
                 (Empire,
                  "deleting " & Destination.Name
                  & " from unexplored because it is owned by "
                  & Destination.Owner.Name);
               Skip := True;
            else
               for Position in Player.Idle_Ship_List.Iterate loop
                  declare
                     use Concorde.Ships;
                     Ship : constant Ship_Type := Element (Position);
                     D    : constant Natural := 1;
--                                Empire.Path_Length
--                                  (Ship.Current_System, Destination);
                  begin
                     if Ship.Current_System = Destination then
                        Concorde.Empires.Logging.Log
                          (Empire,
                           "deleting " & Destination.Name
                           & " from unexplored because of "
                           & Ship.Short_Description);
                        Skip := True;
                        exit;
                     elsif D < Least_Steps then
                        Least_Steps := D;
                        Closest_Ship := Position;
                     end if;
                  end;
               end loop;
            end if;

            pragma Assert (Skip or else Has_Element (Closest_Ship));

            if not Skip then
               Player.Order_Explore_System
                 (Concorde.Empires.Db.Reference (Empire),
                  Element (Closest_Ship),
                  Destination);
               Player.Idle_Ship_List.Delete (Closest_Ship);
            end if;

            Player.Unexplored_Targets.Delete_First;
         end;
      end loop;

      for Unexplored of Player.Unexplored_Targets loop
         Concorde.Empires.Logging.Log
           (Empire,
            "no spare ships to explore "
            & Unexplored.Name);
      end loop;

   end Check_Idle_Ships;

   ------------
   -- Create --
   ------------

   function Create return Player_Type is
   begin
      return Result : constant Player_Type := new Root_Simple_Player_Type do
         null;
      end return;
   end Create;

   ---------------
   -- Idle_Ship --
   ---------------

   procedure Idle_Ship
     (Player : in out Root_Simple_Player_Type'Class;
      Ship   : not null access constant
        Concorde.Ships.Root_Ship_Type'Class)
   is
   begin
      Player.Idle_Ship_List.Append
        (Concorde.Ships.Ship_Type (Ship));
   end Idle_Ship;

   ---------------------
   -- On_Ship_Arrived --
   ---------------------

   overriding procedure On_Ship_Arrived
     (Player : in out Root_Simple_Player_Type;
      Empire : not null access constant
        Concorde.Empires.Root_Empire_Type'Class;
      Ship   : Concorde.Ships.Ship_Type)
   is null;
--     begin
--        if not Ship.Has_Orders then
--           if not Ship.System.Owned then
--              Concorde.Empires.Logging.Log
--                (Empire,
--                 Ship.Short_Description
--                 & ": start colonisation");
--              Player.Order_Colonisation (Empire, Ship);
--           else
--              Concorde.Empires.Logging.Log
--                (Empire,
--                 Ship.Short_Description
--                 & ": arrived");
--              Player.Idle_Ship (Ship);
--              Player.Check_Idle_Ships (Empire.all);
--           end if;
--        end if;
--     end On_Ship_Arrived;

   -----------------------
   -- On_Ship_Completed --
   -----------------------

   overriding procedure On_Ship_Completed
     (Player : in out Root_Simple_Player_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class;
      Ship   : Concorde.Ships.Ship_Type)
   is
   begin
      Player.Idle_Ship (Ship);
      Player.Check_Idle_Ships (Empire);
   end On_Ship_Completed;

   --------------
   -- On_Start --
   --------------

   overriding procedure On_Start
     (Player : in out Root_Simple_Player_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class)
   is
      use Concorde.Galaxy;
      Ns : constant Array_Of_Star_Systems :=
             Neighbours (Empire.Capital.System);
   begin
      for N of Ns loop
         if not N.Owned then
            Player.Unexplored_Targets.Append (N);
--            Empire.Set (N, Concorde.Empires.Claim);
         end if;
      end loop;
--        Empire.Set (Empire.Capital.System, Concorde.Empires.Owned);
--        Empire.Set (Empire.Capital.System, Concorde.Empires.Claim);
      Player.Owned_Systems.Append (Empire.Capital.System);
   end On_Start;

   -------------------------
   -- On_System_Colonised --
   -------------------------

   overriding procedure On_System_Colonised
     (Player : in out Root_Simple_Player_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class;
      System : Concorde.Systems.Root_Star_System_Type'Class;
      Ship   : not null access constant
        Concorde.Ships.Root_Ship_Type'Class)
   is
   begin
      Player.Owned_Systems.Append
        (Concorde.Systems.Db.Reference (System));

      Player.Idle_Ship (Ship);

      for N of Concorde.Galaxy.Neighbours (System) loop
--           if not N.Owned
--             and then not Empire.Is_Set (N, Concorde.Empires.Claim)
--           then
            Player.Unexplored_Targets.Append (N);
--              Empire.Set (N, Concorde.Empires.Claim);
--           end if;
      end loop;

      Player.Check_Idle_Ships (Empire);

   end On_System_Colonised;

end Concorde.Players.Simple_Player;
