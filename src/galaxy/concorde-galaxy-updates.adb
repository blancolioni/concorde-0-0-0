with Concorde.Ships.Updates;
with Concorde.Worlds.Updates;

with Concorde.Galaxy.Ships;

with Concorde.Factions;
with Concorde.Factions.Logging;
with Concorde.Factions.Relations;

with Concorde.Ships.Battles;
with Concorde.Ships.Lists;

package body Concorde.Galaxy.Updates is

   -------------------
   -- Update_Galaxy --
   -------------------

   procedure Update_Galaxy is

      procedure Update_World
        (World : Concorde.Worlds.World_Type);

      ------------------
      -- Update_World --
      ------------------

      procedure Update_World
        (World : Concorde.Worlds.World_Type)
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
                  Es : constant Concorde.Factions.Array_Of_Factions :=
                         Concorde.Ships.Battles.Factions_Present (List);
               begin
                  if not World.Owned then
                     if Es'Length = 1 then
                        --  colonise unclaimed world
                        null;
--                          declare
--                             Coloniser : Concorde.Ships.Ship_Type;
--                     New_Owner : constant Concorde.Factions.Faction_Type :=
--                                           Es (Es'First);
--
--                             procedure Update_Owner
--                        (Faction : in out Factions.Root_Faction_Type'Class);
--
--                             ------------------
--                             -- Update_Owner --
--                             ------------------
--
--                             procedure Update_Owner
--                          (Faction : in out Factions.Root_Faction_Type'Class)
--                             is
--                             begin
--                                Faction.World_Acquired (World);
--                                Faction.Player.On_World_Colonised
--                                  (Faction, World, Coloniser);
--                             end Update_Owner;
--
--                          begin
--                             for Ship of List loop
--                                if Ship.Has_Colonisation_Order then
--                                   Concorde.Factions.Logging.Log
--                                     (New_Owner,
--                                      "colonises " & World.Name);
--                                   Coloniser := Ship;
--                                   Concorde.Ships.Db.Update
--                                     (Ship.Reference,
--                                      Concorde.Ships.Clear_Orders'Access);
--                                   World.Set_Owner (New_Owner);
--                                   Concorde.Factions.Db.Update
--                                 (New_Owner.Reference, Update_Owner'Access);
--                                   exit;
--                                end if;
--                             end loop;
--                          end;
                     end if;
                  elsif not Concorde.Factions.Relations.Has_Conflict (Es) then
                     declare
                        use Concorde.Factions, Concorde.Factions.Relations;
                        Current_Owner : constant Faction_Type := World.Owner;
                        New_Owner     : Faction_Type := null;
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
--                             Current_Owner.Update.World_Lost (World);
--                             New_Owner.Update.World_Acqured (World);

                           World.Update.Set_Owner (New_Owner);
                           Concorde.Factions.Logging.Log
                             (Current_Owner,
                              "loses control of " & World.Name & " to "
                              & New_Owner.Name);
                           Concorde.Factions.Logging.Log
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

--        Concorde.Factions.Clear_Battles;

      Concorde.Worlds.Scan_Worlds (Update_World'Access);

      if False then
         Concorde.Galaxy.Ships.Start_Ship_Moves;
         Concorde.Ships.Updates.Update_Ship_Movement;
         Concorde.Galaxy.Ships.Commit_Ship_Moves;
      end if;

   end Update_Galaxy;

end Concorde.Galaxy.Updates;
