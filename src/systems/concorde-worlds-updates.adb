with Concorde.Agents;

with Concorde.Empires;
--  with Concorde.Empires.Db;
--  with Concorde.Empires.Logging;

with Concorde.Government.Db;

--  with Concorde.Ships.Create;
--  with Concorde.Ships.Db;

with Concorde.Markets.Db;

--  with Concorde.Players;

package body Concorde.Worlds.Updates is

--     Base_Loyalty_Change : constant := 0.002;

   --------------------
   -- Execute_Trades --
   --------------------

   procedure Execute_Trades (World : Root_World_Type'Class) is

      procedure Execute
        (Government : in out Concorde.Government.Root_Government_Type'Class);

      -------------
      -- Execute --
      -------------

      procedure Execute
        (Government : in out Concorde.Government.Root_Government_Type'Class)
      is

         procedure Execute_Market
           (Market : in out Concorde.Markets.Root_Market_Type'Class);

         --------------------
         -- Execute_Market --
         --------------------

         procedure Execute_Market
           (Market : in out Concorde.Markets.Root_Market_Type'Class)
         is
         begin
            Market.Execute (Government);
         end Execute_Market;

      begin
         Concorde.Markets.Db.Update
           (World.Market.Reference, Execute_Market'Access);
      end Execute;

   begin
      if World.Has_Government then
         Concorde.Government.Db.Update
           (World.Government.Reference, Execute'Access);
         Concorde.Markets.Db.Update
           (World.Market.Reference, Concorde.Markets.After_Trading'Access);
      end if;
   end Execute_Trades;

   -------------------
   -- Update_World --
   -------------------

   procedure Update_World (World : in out Root_World_Type'Class) is null;

--     begin
--        if World.Owned then
--           if World.Loyalty < 1.0 then
--              if 1.0 - World.Loyalty <= Base_Loyalty_Change then
--                 World.Loyalty := 1.0;
--                 World.Original_Owner := null;
--              else
--                 World.Loyalty := World.Loyalty + Base_Loyalty_Change;
--              end if;
--           end if;
--           declare
--              Available : constant Non_Negative_Real :=
--                            World.Production * World.Loyalty;
--              Con       : Non_Negative_Real;
--              Dmg       : Natural := 0;
--              Pts       : Positive;
--           begin
--              for Ship of World.Ships loop
--                 if not Ship.Has_Destination
--                   and then Ship.Alive
--                   and then Ship.Damage > 0.0
--                 then
--                    Dmg := Dmg + 1;
--                 end if;
--              end loop;
--
--              Con := Available / Real (Dmg + 1);
--              Pts := Natural'Max (Natural (Con * 100.0), 1);
--
--              if Dmg > 0 then
--                 for Ship of World.Ships loop
--                    declare
--                       procedure Repair
--                         (Ship : in out Concorde.Ships.Root_Ship_Type'Class);
--
--                       ------------
--                       -- Repair --
--                       ------------
--
--                       procedure Repair
--                         (Ship : in out Concorde.Ships.Root_Ship_Type'Class)
--                       is
--                       begin
--                          Ship.Repair (Pts);
--                       end Repair;
--
--                    begin
--                       if not Ship.Has_Destination
--                         and then Ship.Alive
--                         and then Ship.Damage > 0.0
--                       then
--                          Concorde.Empires.Logging.Log
--                            (Ship.Owner,
--                             Ship.Short_Description
--                             & ": repairing" & Pts'Img);
--                          Concorde.Ships.Db.Update
--                            (Ship.Reference, Repair'Access);
--                          Concorde.Empires.Logging.Log
--                            (Ship.Owner,
--                             Ship.Short_Description
--                             & ": repaired");
--                       end if;
--                    end;
--                 end loop;
--              end if;
--
--              World.Progress := World.Progress + Con;
--           end;
--
--           if World.Progress >= 1.0 then
--              declare
--                 Ship : constant Concorde.Ships.Ship_Type :=
--                          Concorde.Ships.Create.New_Ship
--                            (Owner  => World.Owner,
--                             Name   => "",
--                             World => World,
--                             Design => World.Owner.Default_Ship_Design);
--
--                 procedure Notify
--                   (Empire : in out Concorde.Empires.Root_Empire_Type'Class);
--
--                 ------------
--                 -- Notify --
--                 ------------
--
--                 procedure Notify
--                   (Empire : in out Concorde.Empires.Root_Empire_Type'Class)
--                 is
--                 begin
--                    Empire.Player.On_Ship_Completed
--                      (Empire, Ship);
--                 end Notify;
--
--              begin
--                 World.Ships.Append (Ship);
--                 Concorde.Empires.Db.Update
--                   (World.Owner.Reference, Notify'Access);
--              end;
--
--              World.Progress := World.Progress - 1.0;
--           end if;
--        end if;
--   end Update_World;

   ------------------------------
   -- Update_World_Government --
   ------------------------------

   procedure Update_World_Government
     (World : Root_World_Type'Class)
   is null;
--     begin
--
--        if World.Government.Basic_Living_Wage then
--           declare
--              use Concorde.Money;
--              Wage : constant Price_Type :=
--                       World.Market.Current_Price
--                         (Concorde.Commodities.Get ("rations"));
--           begin
--              for Pop of World.Pops loop
--                 declare
--                    Minimum : constant Money_Type :=
--                                Total (Wage, Pop.Size_Quantity);
--                 begin
--                    if Pop.Cash < Minimum then
--                       declare
--                          Payment : constant Money_Type :=
--                                      Minimum - Pop.Cash;
--
--                          procedure Update_Government
--                            (Government : in out
--                           Concorde.Government.Root_Government_Type'Class);
--
--                          procedure Update_Pop
--                            (Pop : in out
--                               Concorde.People.Pops.Root_Pop_Type'Class);
--
--                          -----------------------
--                          -- Update_Government --
--                          -----------------------
--
--                          procedure Update_Government
--                            (Government : in out
--                            Concorde.Government.Root_Government_Type'Class)
--                          is
--                          begin
--                             Government.Remove_Cash (Payment);
--                          end Update_Government;
--
--                          ----------------
--                          -- Update_Pop --
--                          ----------------
--
--                          procedure Update_Pop
--                            (Pop : in out
--                               Concorde.People.Pops.Root_Pop_Type'Class)
--                          is
--                          begin
--                             Pop.Add_Cash (Payment);
--                          end Update_Pop;
--
--                       begin
--                          Pop.Log_Price
--                            ("receive " & Concorde.Money.Image (Payment)
--                             & " dole");
--                          Concorde.People.Pops.Db.Update
--                            (Pop.Reference, Update_Pop'Access);
--                          Concorde.Government.Db.Update
--                            (World.Government.Reference,
--                             Update_Government'Access);
--                       end;
--                    end if;
--                 end;
--              end loop;
--           end;
--        end if;
--
--     end Update_World_Government;

end Concorde.Worlds.Updates;
