with Concorde.Ships.Updates;
with Concorde.Systems.Updates;

with Concorde.Galaxy.Ships;

with Concorde.Empires;
with Concorde.Empires.Db;
with Concorde.Empires.Logging;
with Concorde.Empires.Relations;

with Concorde.Ships.Battles;
with Concorde.Ships.Lists;
with Concorde.Ships.Db;
with Concorde.Systems.Db;

with Concorde.Players;

package body Concorde.Galaxy.Updates is

   -------------------
   -- Update_Galaxy --
   -------------------

   procedure Update_Galaxy is

      procedure Update_System
        (System : in out Concorde.Systems.Root_Star_System_Type'Class);

      -------------------
      -- update_System --
      -------------------

      procedure Update_System
        (System : in out Concorde.Systems.Root_Star_System_Type'Class)
      is
      begin
         Concorde.Systems.Updates.Update_System (System);

         if System.Has_Government then
            Concorde.Systems.Updates.Update_System_Government (System);
         end if;

         declare
            List : Concorde.Ships.Lists.List;
         begin
            System.Get_Ships (List);
            if not List.Is_Empty then
               declare
                  Es : constant Concorde.Empires.Array_Of_Empires :=
                         Concorde.Ships.Battles.Empires_Present (List);
               begin
                  if not System.Owned then
                     if Es'Length = 1 then
                        --  colonise unclaimed world
                        declare
                           Coloniser : Concorde.Ships.Ship_Type;
                           New_Owner : constant Concorde.Empires.Empire_Type :=
                                         Es (Es'First);

                           procedure Update_Owner
                             (Empire : in out Empires.Root_Empire_Type'Class);

                           ------------------
                           -- Update_Owner --
                           ------------------

                           procedure Update_Owner
                             (Empire : in out Empires.Root_Empire_Type'Class)
                           is
                           begin
                              Empire.System_Acquired (System);
                              Empire.Player.On_System_Colonised
                                (Empire, System, Coloniser);
                           end Update_Owner;

                        begin
                           for Ship of List loop
                              if Ship.Has_Colonisation_Order then
                                 Concorde.Empires.Logging.Log
                                   (New_Owner,
                                    "colonises " & System.Name);
                                 Coloniser := Ship;
                                 Concorde.Ships.Db.Update
                                   (Ship.Reference,
                                    Concorde.Ships.Clear_Orders'Access);
                                 System.Set_Owner (New_Owner);
                                 Concorde.Empires.Db.Update
                                   (New_Owner.Reference, Update_Owner'Access);
                                 exit;
                              end if;
                           end loop;
                        end;
                     end if;
                  elsif not Concorde.Empires.Relations.Has_Conflict (Es) then
                     declare
                        use Concorde.Empires, Concorde.Empires.Relations;
                        Current_Owner : constant Empire_Type := System.Owner;
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
                              procedure Update_System_Acquired
                                (Empire : in out Root_Empire_Type'Class);

                              procedure Update_System_Lost
                                (Empire : in out Root_Empire_Type'Class);

                              ----------------------------
                              -- Update_System_Acquired --
                              ----------------------------

                              procedure Update_System_Acquired
                                (Empire : in out Root_Empire_Type'Class)
                              is
                              begin
                                 Empire.System_Acquired (System);
                                 Empire.Player.On_System_Captured
                                   (Empire, System, Current_Owner);
                              end Update_System_Acquired;

                              ------------------------
                              -- Update_System_Lost --
                              ------------------------

                              procedure Update_System_Lost
                                (Empire : in out Root_Empire_Type'Class)
                              is
                              begin
                                 Empire.System_Lost (System);
                                 Empire.Player.On_System_Lost
                                   (Empire, System, New_Owner);
                              end Update_System_Lost;

                           begin
                              Concorde.Empires.Db.Update
                                (Current_Owner.Reference,
                                 Update_System_Lost'Access);

                              Concorde.Empires.Db.Update
                                (New_Owner.Reference,
                                 Update_System_Acquired'Access);

                           end;

                           System.Set_Owner (New_Owner);
                           Concorde.Empires.Logging.Log
                             (Current_Owner,
                              "loses control of " & System.Name & " to "
                              & New_Owner.Name);
                           Concorde.Empires.Logging.Log
                             (New_Owner,
                              "acquires " & System.Name & " from "
                              & Current_Owner.Name);
                        end if;
                     end;
                  end if;
               end;
            end if;
         end;
      end Update_System;

   begin

      Concorde.Empires.Clear_Battles;

      Concorde.Systems.Db.Iterate (Update_System'Access);

      Concorde.Galaxy.Ships.Start_Ship_Moves;
      Concorde.Ships.Updates.Update_Ship_Movement;
      Concorde.Galaxy.Ships.Commit_Ship_Moves;

      Concorde.Systems.Db.Iterate
        (Concorde.Systems.Updates.Update_System'Access);

   end Update_Galaxy;

end Concorde.Galaxy.Updates;
