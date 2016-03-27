with Concorde.Ships.Updates;
with Concorde.Systems.Updates;

with Concorde.Galaxy.Ships;

with Concorde.AI;
with Concorde.Empires;
with Concorde.Empires.Logging;
with Concorde.Empires.Relations;

with Concorde.Ships.Battles;
with Concorde.Ships.Lists;

package body Concorde.Galaxy.Updates is

   -------------------
   -- Update_Galaxy --
   -------------------

   procedure Update_Galaxy is
      Changed_Owners : Boolean := False;
   begin

      Concorde.Empires.Clear_Battles;

      for System of Galaxy_Vector loop
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
                        begin
                           Changed_Owners := True;
                           Concorde.Empires.Logging.Log
                             (New_Owner,
                              "colonises " & System.Name);
                           System.Set_Owner (New_Owner);
                           New_Owner.System_Acquired
                             (Concorde.Systems.Star_System_Type (System));
                           New_Owner.AI.System_Acquired
                             (Concorde.Systems.Star_System_Type (System),
                              null);
                        end;
                     end if;
                  elsif not Concorde.Empires.Relations.Has_Conflict (Es) then
                     declare
                        use Concorde.Empires, Concorde.Empires.Relations;
                        Current_Owner : constant Empire_Type := System.Owner;
                        New_Owner     : Empire_Type := null;
                     begin
                        for E of Es loop
                           if At_War (E, Current_Owner) then
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
                           Current_Owner.System_Lost
                             (Concorde.Systems.Star_System_Type (System));
                           Current_Owner.AI.System_Lost
                             (Concorde.Systems.Star_System_Type (System),
                              New_Owner);
                           New_Owner.System_Acquired
                             (Concorde.Systems.Star_System_Type (System));
                           New_Owner.AI.System_Acquired
                             (Concorde.Systems.Star_System_Type (System),
                              Current_Owner);
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
      end loop;

      if Changed_Owners then
         Update_System_Flags;
      end if;

      Concorde.Galaxy.Ships.Start_Ship_Moves;
      Concorde.Ships.Updates.Update_Ship_Movement;
      Concorde.Galaxy.Ships.Commit_Ship_Moves;

      for System of Galaxy_Vector loop
         Concorde.Systems.Updates.Update_System (System);
      end loop;

   end Update_Galaxy;

   -------------------------
   -- Update_System_Flags --
   -------------------------

   procedure Update_System_Flags is
   begin
      for System of Galaxy_Vector loop

         if System.Owned then
            System.Owner.Clear_System_Flags (System);
            declare
               Border : Boolean := False;
            begin
               for N of Neighbours (System) loop
                  if System.Owner /= N.Owner then
                     Border := True;
                     System.Owner.Set_Neighbour (N, True);
                     if N.Owned then
                        if N.Owner /= System.Owner then
                           System.Owner.Set_Border (System, True);
                           System.Owner.Set_Neighbour (N, True);
                        end if;
                     else
                        System.Owner.Set_Frontier (System, True);
                        System.Owner.Set_Neighbour (N, True);
                     end if;
                  end if;
               end loop;
               if not Border then
                  System.Owner.Set_Internal (System, True);
               end if;
            end;
         end if;
      end loop;

   end Update_System_Flags;

end Concorde.Galaxy.Updates;
