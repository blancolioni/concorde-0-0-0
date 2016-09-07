with Concorde.Ships.Updates;
with Concorde.Worlds.Updates;

with Concorde.Galaxy.Ships;

with Concorde.Empires;
with Concorde.Empires.Db;
with Concorde.Empires.Logging;
with Concorde.Empires.Relations;

with Concorde.Ships.Battles;
with Concorde.Ships.Lists;
--  with Concorde.Ships.Db;
--  with Concorde.Systems.Db;
with Concorde.Worlds.Db;

--  with Concorde.Players;

package body Concorde.Galaxy.Updates is

   -------------------
   -- Update_Galaxy --
   -------------------

   procedure Update_Galaxy is

      procedure Update_World
        (World : in out Concorde.Worlds.Root_World_Type'Class);

      ------------------
      -- Update_World --
      ------------------

      procedure Update_World
        (World : in out Concorde.Worlds.Root_World_Type'Class)
      is
      begin
         Concorde.Worlds.Updates.Update_World (World);

         if World.Has_Government then
            Concorde.Worlds.Updates.Update_World_Government (World);
         end if;

         declare
            List : Concorde.Ships.Lists.List;
         begin
            World.Get_Ships (List);
            if not List.Is_Empty then
               declare
                  Es : constant Concorde.Empires.Array_Of_Empires :=
                         Concorde.Ships.Battles.Empires_Present (List);
               begin
                  if not World.Owned then
                     if Es'Length = 1 then
                        --  colonise unclaimed world
                        null;
--                          declare
--                             Coloniser : Concorde.Ships.Ship_Type;
--                        New_Owner : constant Concorde.Empires.Empire_Type :=
--                                           Es (Es'First);
--
--                             procedure Update_Owner
--                        (Empire : in out Empires.Root_Empire_Type'Class);
--
--                             ------------------
--                             -- Update_Owner --
--                             ------------------
--
--                             procedure Update_Owner
--                          (Empire : in out Empires.Root_Empire_Type'Class)
--                             is
--                             begin
--                                Empire.World_Acquired (World);
--                                Empire.Player.On_World_Colonised
--                                  (Empire, World, Coloniser);
--                             end Update_Owner;
--
--                          begin
--                             for Ship of List loop
--                                if Ship.Has_Colonisation_Order then
--                                   Concorde.Empires.Logging.Log
--                                     (New_Owner,
--                                      "colonises " & World.Name);
--                                   Coloniser := Ship;
--                                   Concorde.Ships.Db.Update
--                                     (Ship.Reference,
--                                      Concorde.Ships.Clear_Orders'Access);
--                                   World.Set_Owner (New_Owner);
--                                   Concorde.Empires.Db.Update
--                                 (New_Owner.Reference, Update_Owner'Access);
--                                   exit;
--                                end if;
--                             end loop;
--                          end;
                     end if;
                  elsif not Concorde.Empires.Relations.Has_Conflict (Es) then
                     declare
                        use Concorde.Empires, Concorde.Empires.Relations;
                        Current_Owner : constant Empire_Type := World.Owner;
                        New_Owner     : Empire_Type := null;
                     begin
                        for E of Es loop
                           if At_War (E.all, Current_Owner.all) then
                              if New_Owner = null then
                                 New_Owner := E;
                              else
                                 New_Owner := null;
                                 exit;
                              end if;
                           end if;
                        end loop;

                        if New_Owner /= null then

                           declare
                              procedure Update_World_Acquired
                                (Empire : in out Root_Empire_Type'Class);

                              procedure Update_World_Lost
                                (Empire : in out Root_Empire_Type'Class);

                              ----------------------------
                              -- Update_World_Acquired --
                              ----------------------------

                              procedure Update_World_Acquired
                                (Empire : in out Root_Empire_Type'Class)
                              is
                              begin

                                 null;

--                                   Empire.World_Acquired (World);
--                                   Empire.Player.On_World_Captured
--                                     (Empire, World, Current_Owner);
                              end Update_World_Acquired;

                              ------------------------
                              -- Update_World_Lost --
                              ------------------------

                              procedure Update_World_Lost
                                (Empire : in out Root_Empire_Type'Class)
                              is
                              begin
                                 null;

--                                   Empire.World_Lost (World);
--                                   Empire.Player.On_World_Lost
--                                     (Empire, World, New_Owner);
                              end Update_World_Lost;

                           begin
                              Concorde.Empires.Db.Update
                                (Current_Owner.Reference,
                                 Update_World_Lost'Access);

                              Concorde.Empires.Db.Update
                                (New_Owner.Reference,
                                 Update_World_Acquired'Access);

                           end;

                           World.Set_Owner (New_Owner);
                           Concorde.Empires.Logging.Log
                             (Current_Owner,
                              "loses control of " & World.Name & " to "
                              & New_Owner.Name);
                           Concorde.Empires.Logging.Log
                             (New_Owner,
                              "acquires " & World.Name & " from "
                              & Current_Owner.Name);
                        end if;
                     end;
                  end if;
               end;
            end if;
         end;
      end Update_World;

   begin

--        Concorde.Empires.Clear_Battles;

      Concorde.Worlds.Db.Iterate (Update_World'Access);

      if False then
         Concorde.Galaxy.Ships.Start_Ship_Moves;
         Concorde.Ships.Updates.Update_Ship_Movement;
         Concorde.Galaxy.Ships.Commit_Ship_Moves;
      end if;

      Concorde.Worlds.Db.Iterate
        (Concorde.Worlds.Updates.Update_World'Access);

   end Update_Galaxy;

end Concorde.Galaxy.Updates;
