with Concorde.Ships.Updates;
with Concorde.Systems.Updates;

with Concorde.Galaxy.Ships;

with Concorde.AI;
with Concorde.Empires;
with Concorde.Empires.Db;
with Concorde.Empires.Logging;
with Concorde.Empires.Relations;

with Concorde.Ships.Battles;
with Concorde.Ships.Lists;

with Concorde.Systems.Db;

package body Concorde.Galaxy.Updates is

   -------------------
   -- Update_Galaxy --
   -------------------

   procedure Update_Galaxy is
      Changed_Owners : Boolean := False;

      procedure Update_System
        (System : in out Concorde.Systems.Root_Star_System_Type'Class);

      -------------------
      -- update_System --
      -------------------

      procedure Update_System
        (System : in out Concorde.Systems.Root_Star_System_Type'Class)
      is
         Reference : constant Concorde.Systems.Star_System_Type :=
                       Concorde.Systems.Db.Reference (System);
      begin
         Concorde.Systems.Updates.Update_System (System);
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
                              Empire.AI.System_Acquired
                                (Empire, Reference, null);
                           end Update_Owner;

                        begin
                           Changed_Owners := True;
                           Concorde.Empires.Logging.Log
                             (New_Owner,
                              "colonises " & System.Name);
                           System.Set_Owner (New_Owner);
                           Concorde.Empires.Db.Update
                             (New_Owner.Reference, Update_Owner'Access);

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
                           Changed_Owners := True;

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
                                 Empire.AI.System_Acquired
                                   (Empire, Reference, Current_Owner);
                              end Update_System_Acquired;

                              ------------------------
                              -- Update_System_Lost --
                              ------------------------

                              procedure Update_System_Lost
                                (Empire : in out Root_Empire_Type'Class)
                              is
                              begin
                                 Empire.System_Lost (System);
                                 Empire.AI.System_Lost
                                   (Empire, Reference, New_Owner);
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

      if Changed_Owners then
         Update_System_Flags;
      end if;

      Concorde.Galaxy.Ships.Start_Ship_Moves;
      Concorde.Ships.Updates.Update_Ship_Movement;
      Concorde.Galaxy.Ships.Commit_Ship_Moves;

      Concorde.Systems.Db.Iterate
        (Concorde.Systems.Updates.Update_System'Access);

   end Update_Galaxy;

   -------------------------
   -- Update_System_Flags --
   -------------------------

   procedure Update_System_Flags is

      procedure Process
        (System : Concorde.Systems.Star_System_Type);

      -------------
      -- Process --
      -------------

      procedure Process
        (System : Concorde.Systems.Star_System_Type)
      is
         procedure Update_Owner
           (Owner : in out Concorde.Empires.Root_Empire_Type'Class);

         procedure Update_Owner
           (Owner : in out Concorde.Empires.Root_Empire_Type'Class)
         is
            Border : Boolean := False;
         begin
            Owner.Clear_System_Flags (System);

            for N of Neighbours (System) loop
               if System.Owner /= N.Owner then
                  Border := True;
                  Owner.Set (N, Concorde.Empires.Neighbour);
                  if N.Owned then
                     if N.Owner /= System.Owner then
                        Owner.Set (System, Concorde.Empires.Border);
                        Owner.Set (N, Concorde.Empires.Neighbour);
                     end if;
                  else
                     Owner.Set (System, Concorde.Empires.Frontier);
                     Owner.Set (N, Concorde.Empires.Neighbour);
                  end if;
               end if;
            end loop;
            if not Border then
               Owner.Set (System, Concorde.Empires.Internal);
            end if;
         end Update_Owner;

      begin
         if System.Owned then
            Concorde.Empires.Db.Update
              (System.Owner.Reference, Update_Owner'Access);
         end if;
      end Process;

   begin

      Concorde.Systems.Db.Scan (Process'Access);

   end Update_System_Flags;

end Concorde.Galaxy.Updates;
