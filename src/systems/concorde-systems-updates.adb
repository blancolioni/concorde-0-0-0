with Concorde.Empires;
with Concorde.Empires.Db;
with Concorde.Empires.Logging;

with Concorde.Ships.Create;
with Concorde.Ships.Db;

with Concorde.Players;

package body Concorde.Systems.Updates is

   Base_Loyalty_Change : constant := 0.002;

   -------------------
   -- Update_System --
   -------------------

   procedure Update_System (System : in out Root_Star_System_Type'Class) is
   begin
      if System.Owned then
         if System.Loyalty < 1.0 then
            if 1.0 - System.Loyalty <= Base_Loyalty_Change then
               System.Loyalty := 1.0;
               System.Original_Owner := null;
            else
               System.Loyalty := System.Loyalty + Base_Loyalty_Change;
            end if;
         end if;
         declare
            Available : constant Unit_Real :=
                          System.Production * System.Loyalty;
            Con       : Unit_Real;
            Dmg       : Natural := 0;
            Pts       : Positive;
         begin
            for Ship of System.Ships loop
               if not Ship.Has_Destination
                 and then Ship.Alive
                 and then Ship.Damage > 0.0
               then
                  Dmg := Dmg + 1;
               end if;
            end loop;

            Con := Available / Real (Dmg + 1);
            Pts := Natural'Max (Natural (Con * 100.0), 1);

            if Dmg > 0 then
               for Ship of System.Ships loop
                  declare
                     procedure Repair
                       (Ship : in out Concorde.Ships.Root_Ship_Type'Class);

                     ------------
                     -- Repair --
                     ------------

                     procedure Repair
                       (Ship : in out Concorde.Ships.Root_Ship_Type'Class)
                     is
                     begin
                        Ship.Repair (Pts);
                     end Repair;

                  begin
                     if not Ship.Has_Destination
                       and then Ship.Alive
                       and then Ship.Damage > 0.0
                     then
                        Concorde.Empires.Logging.Log
                          (Ship.Owner,
                           Ship.Short_Description
                           & ": repairing" & Pts'Img);
                        Concorde.Ships.Db.Update
                          (Ship.Reference, Repair'Access);
                        Concorde.Empires.Logging.Log
                          (Ship.Owner,
                           Ship.Short_Description
                           & ": repaired");
                     end if;
                  end;
               end loop;
            end if;

            System.Progress := System.Progress + Con;
         end;

         if System.Progress >= 1.0 then
            if System.Owner.Available_Ship_Capacity > 0 then
               declare
                  Ship : constant Concorde.Ships.Ship_Type :=
                           Concorde.Ships.Create.New_Ship
                             (Owner  => System.Owner,
                              System => System,
                              Design => System.Owner.Default_Ship_Design);

                  procedure Notify
                    (Empire : in out Concorde.Empires.Root_Empire_Type'Class);

                  ------------
                  -- Notify --
                  ------------

                  procedure Notify
                    (Empire : in out Concorde.Empires.Root_Empire_Type'Class)
                  is
                  begin
                     Empire.Player.On_Ship_Completed
                       (Empire, Ship);
                  end Notify;

               begin
                  System.Ships.Append (Ship);
                  Concorde.Empires.Db.Update
                    (System.Owner.Reference, Notify'Access);
               end;

               System.Progress := System.Progress - 1.0;
            else
               System.Progress := 1.0;
            end if;
         end if;
      end if;
   end Update_System;

end Concorde.Systems.Updates;
