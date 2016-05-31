with Concorde.Galaxy;
with Concorde.Locations;
with Concorde.Markets;
with Concorde.Ships.Create;
with Concorde.Stars;
with Concorde.Worlds;
with Concorde.Systems;

with Concorde.Scenarios;

with Concorde.Colonies.Configure;

with Concorde.Empires.Db;
with Concorde.Markets.Db;
with Concorde.Ships.Db;

with Concorde.Commodities;
with Concorde.Money;
with Concorde.Quantities;

with Concorde.Worlds.Db;

package body Concorde.Empires.Create is

   procedure Create_Initial_Ships
     (World : in out Concorde.Worlds.Root_World_Type'Class);

   --------------------------
   -- Create_Initial_Ships --
   --------------------------

   procedure Create_Initial_Ships
     (World : in out Concorde.Worlds.Root_World_Type'Class)
   is
      Trader   : constant Concorde.Ships.Ship_Type :=
                   Concorde.Ships.Create.New_Ship
                     (Owner  => World.Owner,
                      Name   =>
                        World.Owner.Name
                      & " Trader",
                      World  => World,
                      Design => "trader");
      Defender : constant Concorde.Ships.Ship_Type :=
                   Concorde.Ships.Create.New_Ship
                     (Owner  => World.Owner,
                      Name   =>
                        World.Owner.Name
                      & " Defender",
                      World  => World,
                      Design => "defender");

      procedure Initial_Trade_Route
        (Ship : in out Concorde.Ships.Root_Ship_Type'Class);

      -------------------------
      -- Initial_Trade_Route --
      -------------------------

      procedure Initial_Trade_Route
        (Ship : in out Concorde.Ships.Root_Ship_Type'Class)
      is
         Start    : constant Concorde.Worlds.World_Type :=
                      Concorde.Worlds.Db.Reference (World);
         Capital  : constant Concorde.Worlds.World_Type :=
                      Galaxy.Capital_World;
         Consumer : constant Commodities.Array_Of_Commodities :=
                      Commodities.Get (Commodities.Consumer);
         Resources : constant Commodities.Array_Of_Commodities :=
                       World.Resources;
         Organics  : constant Commodities.Array_Of_Commodities :=
                       Concorde.Commodities.Get
                         (Concorde.Commodities.Organic);
      begin
         for Item of Consumer loop
            Ship.Add_Sell_Order (Start, Item);
         end loop;

         for Item of Organics loop
            Ship.Add_Buy_Order (Start, Item);
         end loop;

         for R of Resources loop
            Ship.Add_Buy_Order (Start, R);
         end loop;

         for Item of Organics loop
            Ship.Add_Sell_Order (Capital, Item);
         end loop;

         for R of Resources loop
            Ship.Add_Sell_Order (Capital, R);
         end loop;

         for Item of Consumer loop
            Ship.Add_Buy_Order (Capital, Item);
         end loop;
         Ship.Cycle_Orders (True);
      end Initial_Trade_Route;

   begin
      Concorde.Ships.Db.Update
        (Trader.Reference, Initial_Trade_Route'Access);

      World.Add_Ship (Trader);
      World.Add_Ship (Defender);
   end Create_Initial_Ships;

   ----------------
   -- New_Empire --
   ----------------

   function New_Empire
     (Name                : String;
      Capital             : String;
      Colour              : Lui.Colours.Colour_Type;
      Default_Ship_Design : String;
      Player              : Concorde.Players.Player_Type)
      return Empire_Type
   is

      Taken : Concorde.Galaxy.Star_System_Set;

      procedure Add_Taken_Systems
        (Empire : Root_Empire_Type'Class);

      procedure Create
        (New_Empire : in out Root_Empire_Type'Class);

      -----------------------
      -- Add_Taken_Systems --
      -----------------------

      procedure Add_Taken_Systems
        (Empire : Root_Empire_Type'Class)
      is
      begin
         Concorde.Galaxy.Add_Systems (Taken, Empire.Capital.System, 0.4);
      end Add_Taken_Systems;

      ------------
      -- Create --
      ------------

      procedure Create
        (New_Empire : in out Root_Empire_Type'Class)
      is

         Start_World : Concorde.Worlds.World_Type;
         Start_System : Concorde.Systems.Star_System_Type;

         procedure Choose_System
           (System : in out Concorde.Systems.Root_Star_System_Type'Class);

         procedure Choose_World
           (World : in out Concorde.Worlds.Root_World_Type'Class);

         function OK_For_Start
           (System : Concorde.Systems.Star_System_Type)
            return Boolean;

         -------------------
         -- Choose_System --
         -------------------

         procedure Choose_System
           (System : in out Concorde.Systems.Root_Star_System_Type'Class)
         is
            use Concorde.Commodities;
         begin
            System.Set_Owner (Db.Reference (New_Empire));
            System.Set_Capital (True);
            System.Set_Name (Capital);
         end Choose_System;

         ------------------
         -- Choose_World --
         ------------------

         procedure Choose_World
           (World : in out Concorde.Worlds.Root_World_Type'Class)
         is
            use Concorde.Commodities;
            Resources : constant Concorde.Commodities.Array_Of_Commodities :=
                          Concorde.Commodities.Get
                            (Concorde.Commodities.Resource);
            Imperial_Centre : constant Boolean :=
                                Concorde.Scenarios.Imperial_Centre
                                    and then Start_System.Index = 1;

            procedure Set_Initial_Prices
              (Market : in out Concorde.Markets.Root_Market_Type'Class);

            ------------------------
            -- Set_Initial_Prices --
            ------------------------

            procedure Set_Initial_Prices
              (Market : in out Concorde.Markets.Root_Market_Type'Class)
            is
            begin
               for Unavailable of Resources loop
                  if Imperial_Centre then
                     Market.Initial_Price
                       (Unavailable,
                        Concorde.Money.Adjust_Price
                          (Unavailable.Base_Price, Factor => 2.0));
               --                 elsif Unavailable /= System.Resource then
               --                    System.Market.Initial_Price
               --                      (Unavailable,
               --                       Concorde.Money.Adjust_Price
               --                   (Unavailable.Base_Price, Factor => 2.0));
                  end if;
               end loop;

               declare
                  Consumer_Goods : constant Array_Of_Commodities :=
                                     Get (Consumer);
               begin
                  for Item of Consumer_Goods loop
                     if Imperial_Centre then
                        null;
                     else
                        Market.Initial_Price
                          (Item,
                           Concorde.Money.Adjust_Price
                             (Item.Base_Price, Factor => 2.0));
                     end if;
                  end loop;
               end;

            end Set_Initial_Prices;

         begin
            World.Set_Owner (Db.Reference (New_Empire));
            World.Set_Capital (True);
            World.Set_Name (Capital);

            if Concorde.Scenarios.Imperial_Centre
              and then Start_System.Index = 1
            then
               Concorde.Colonies.Configure.Create_Colony_From_Template
                 (World, "imperial_capital");
            else
               Concorde.Colonies.Configure.Create_Colony_From_Template
                 (World, "initial");

               Create_Initial_Ships (World);

            end if;

            Concorde.Markets.Db.Update
              (World.Market.Reference, Set_Initial_Prices'Access);

         end Choose_World;

         ------------------
         -- OK_For_Start --
         ------------------

         function OK_For_Start
           (System : Concorde.Systems.Star_System_Type)
         return Boolean
         is
            use Concorde.Galaxy;
            Ns : constant Array_Of_Star_Systems :=
                   Neighbours (System);
         begin
--              if Imperial_Centre and then System.Index = 1 then
--                 return False;
--              end if;

            if System.Owner /= null then
               return False;
            end if;

            if Concorde.Galaxy.Is_Element (Taken, System) then
               return False;
            end if;

            for S of Ns loop
               if S.Owner /= null then
                  return False;
               end if;
            end loop;

            declare
               Good_Starting_World : Boolean := False;

               procedure Check_World
                 (System_Object : Systems.Star_System_Object_Interface'Class);

               -----------------
               -- Check_World --
               -----------------

               procedure Check_World
                 (System_Object : Systems.Star_System_Object_Interface'Class)
               is
                  use Concorde.Worlds;
               begin
                  if System_Object in Root_World_Type'Class then
                     declare
                        W : Root_World_Type'Class renames
                              Root_World_Type'Class (System_Object);
                     begin
                        if W.Category = Terrestrial then
                           Good_Starting_World := True;
                           Start_World :=
                             Concorde.Worlds.Db.Reference (W);
                        end if;
                     end;
                  end if;
               end Check_World;

            begin
               System.Scan_System_Objects (Check_World'Access);

               if not Good_Starting_World then
                  return False;
               end if;

            end;

            case Concorde.Stars.Star_Type (System.Main_Object).Stellar_Class is
               when Concorde.Stars.G =>
                  return True;
               when others =>
                  return False;
            end case;
         end OK_For_Start;

      begin
         Start_System :=
           Concorde.Galaxy.Find_System
             (OK_For_Start'Access);

         New_Empire.New_Agent
           (Concorde.Locations.Nowhere,
            null,
            Concorde.Quantities.Zero);

         New_Empire.Set_Cash (Concorde.Money.To_Money (1_000_000.0));
         New_Empire.Identifier :=
           Ada.Strings.Unbounded.To_Unbounded_String (Name);
         New_Empire.Set_Name (Name);
         New_Empire.System_Data :=
           new System_Data_Array (1 .. Galaxy.System_Count);
         New_Empire.Colour := Colour;
         New_Empire.Capital_World := Start_World;

         if Concorde.Scenarios.Imperial_Centre
           and then Start_System.Index = 1
         then
            Galaxy.Set_Capital_World (Start_World);
         end if;

         New_Empire.Player := Player;
         New_Empire.Current_Systems := 1;
         New_Empire.Default_Ship := new String'(Default_Ship_Design);
         Concorde.Galaxy.Update_System
           (Start_System, Choose_System'Access);
         Concorde.Worlds.Db.Update
           (Start_World.Reference, Choose_World'Access);

         if False
           and then Concorde.Scenarios.Imperial_Centre
           and then not Concorde.Galaxy.Neighbours (1, Start_System.Index)
         then
            Concorde.Galaxy.Connect (1, Start_System.Index);
         end if;

      end Create;

   begin
      Db.Scan (Add_Taken_Systems'Access);
      return Db.Create (Create'Access);
   end New_Empire;

end Concorde.Empires.Create;
