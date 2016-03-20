with Concorde.Ships.Updates;
with Concorde.Systems.Updates;

with Concorde.Galaxy.Ships;

with Concorde.AI;
with Concorde.Empires;
with Concorde.Empires.Logging;

with Concorde.Ships.Battles;
with Concorde.Ships.Lists;

package body Concorde.Galaxy.Updates is

   -------------------
   -- Update_Galaxy --
   -------------------

   procedure Update_Galaxy is
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
                        end if;
                     else
                        System.Owner.Set_Frontier (System, True);
                     end if;
                  end if;
               end loop;
               if not Border then
                  System.Owner.Set_Internal (System, True);
               end if;
            end;
         end if;
      end loop;

      for System of Galaxy_Vector loop
         Concorde.Systems.Updates.Update_System (System);
         declare
            List : Concorde.Ships.Lists.List;
         begin
            System.Get_Ships (List);
            if not List.Is_Empty
              and then not Concorde.Ships.Battles.Has_Conflict (List)
            then
               declare
                  use Concorde.Empires;
                  Current_Owner : constant Empire_Type :=
                                    Empire_Type (System.Owner);
                  Ship_Owner    : constant Empire_Type :=
                                    Empire_Type (List.First_Element.Owner);
               begin
                  if Current_Owner /= Ship_Owner then
                     if Current_Owner /= null then
                        Current_Owner.System_Lost
                          (Concorde.Systems.Star_System_Type (System));
                        Current_Owner.AI.System_Lost
                          (Concorde.Systems.Star_System_Type (System),
                           Ship_Owner);
                        Concorde.Empires.Logging.Log
                          (Ship_Owner,
                           "acquires " & System.Name & " from "
                           & Current_Owner.Name);
                     end if;
                     System.Set_Owner (Ship_Owner);
                     Ship_Owner.System_Acquired
                       (Concorde.Systems.Star_System_Type (System));
                     Ship_Owner.AI.System_Acquired
                       (Concorde.Systems.Star_System_Type (System),
                        Current_Owner);
                  end if;
               end;
            end if;
         end;
      end loop;

      Concorde.Galaxy.Ships.Start_Ship_Moves;
      Concorde.Ships.Updates.Update_Ship_Movement;
      Concorde.Galaxy.Ships.Commit_Ship_Moves;

      for System of Galaxy_Vector loop
         Concorde.Systems.Updates.Update_System (System);
      end loop;

   end Update_Galaxy;

end Concorde.Galaxy.Updates;
