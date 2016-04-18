with Concorde.Agents;

with Concorde.Empires;
with Concorde.Empires.Db;
with Concorde.Empires.Logging;

with Concorde.Ships.Create;
with Concorde.Ships.Db;

with Concorde.Installations.Production;
with Concorde.Installations.Db;

with Concorde.People.Pops.Db;

with Concorde.Players;

with Concorde.Money;

package body Concorde.Systems.Updates is

   Base_Loyalty_Change : constant := 0.002;

   ------------------------
   -- Execute_Production --
   ------------------------

   procedure Execute_Production (System : Root_Star_System_Type'Class) is

      procedure Execute
        (Installation : in out
           Concorde.Installations.Root_Installation_Type'Class);

      -------------
      -- Execute --
      -------------

      procedure Execute
        (Installation : in out
           Concorde.Installations.Root_Installation_Type'Class)
      is
      begin
         Concorde.Installations.Production.Execute_Production
           (System, Installation);
      end Execute;

   begin
      for Installation of System.Installations loop
         Concorde.Installations.Db.Update
           (Installation.Reference, Execute'Access);
      end loop;
   end Execute_Production;

   --------------------
   -- Execute_Trades --
   --------------------

   procedure Execute_Trades (System : Root_Star_System_Type'Class) is
   begin
      System.Market.Execute;
   end Execute_Trades;

   -------------------
   -- Update_Market --
   -------------------

   procedure Update_Market (System : Root_Star_System_Type'Class) is
      use type Concorde.Installations.Installation_Type;

      Virtual : constant Concorde.Commodities.Array_Of_Commodities :=
                  Concorde.Commodities.Get
                    (Concorde.Commodities.Virtual);

      procedure Clear_Virtual_Stock
        (Rec : in out Memor.Root_Record_Type'Class);

      -------------------------
      -- Clear_Virtual_Stock --
      -------------------------

      procedure Clear_Virtual_Stock
        (Rec : in out Memor.Root_Record_Type'Class)
      is
         Agent : Concorde.Agents.Root_Agent_Type'Class renames
                   Concorde.Agents.Root_Agent_Type'Class (Rec);
      begin
         for Commodity of Virtual loop
            Agent.Set_Quantity (Commodity, Quantities.Zero, Money.Zero);
         end loop;
      end Clear_Virtual_Stock;

   begin
      for Pop of System.Pops loop
         Concorde.People.Pops.Db.Update
           (Pop.Reference, Clear_Virtual_Stock'Access);
         Pop.Add_Trade_Offers (System.Market.all);
      end loop;

      for Installation of System.Installations loop
         Concorde.Installations.Db.Update
           (Installation.Reference, Clear_Virtual_Stock'Access);
         if not Installation.Is_Colony_Hub then
            Installation.Add_Trade_Offers (System.Market.all);
         end if;
      end loop;
      if System.Hub /= null then
         System.Hub.Add_Trade_Offers (System.Market.all);
      end if;
   end Update_Market;

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
