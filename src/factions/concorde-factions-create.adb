with Ada.Text_IO;

with Concorde.Real_Images;

with Concorde.Galaxy;
with Concorde.Locations;
with Concorde.Markets;
with Concorde.Ships.Create;
with Concorde.Stars;
with Concorde.Worlds;
with Concorde.Systems;

with Concorde.Scenarios;

with Concorde.Colonies.Configure;

with Concorde.Commodities;
with Concorde.Money;
with Concorde.Quantities;

with Concorde.Systems.Lists;

package body Concorde.Factions.Create is

   Imperial_Centre : Boolean := True;

   procedure Create_Initial_Ships
     (World : Concorde.Worlds.World_Type);

   function Find_System
     (Start  : Concorde.Systems.Star_System_Type;
      OK     : not null access
        function (System : Concorde.Systems.Star_System_Type)
      return Boolean)
     return Concorde.Systems.Star_System_Type;

   --------------------------
   -- Create_Initial_Ships --
   --------------------------

   procedure Create_Initial_Ships
     (World : Concorde.Worlds.World_Type)
   is
      Defender : constant Concorde.Ships.Ship_Type :=
                   Concorde.Ships.Create.New_Ship
                     (Owner  => World.Owner,
                      Name   =>
                        World.Owner.Name
                      & " Defender",
                      World  => World,
                      Design => "defender");

      procedure Initial_Trade_Route
        (Ship : Concorde.Ships.Ship_Type)
        with Unreferenced;

      -------------------------
      -- Initial_Trade_Route --
      -------------------------

      procedure Initial_Trade_Route
        (Ship : Concorde.Ships.Ship_Type)
      is
         Capital  : constant Concorde.Worlds.World_Type :=
                      Galaxy.Capital_World;
         Food     : constant Commodities.Array_Of_Commodities :=
                      Commodities.Get (Commodities.Food);
         Clothing  : constant Commodities.Array_Of_Commodities :=
                       Commodities.Get (Commodities.Clothing);
         Resources : constant Commodities.Array_Of_Commodities :=
                       World.Resources;
         Organics  : constant Commodities.Array_Of_Commodities :=
                       Concorde.Commodities.Get
                         (Concorde.Commodities.Organic);
      begin

         if True then
            Ship.Update.Set_Trade_Route (World, Capital);
         else

            for Item of Food loop
               Ship.Update.Add_Sell_Order (World, Item);
            end loop;

            for Item of Clothing loop
               Ship.Update.Add_Sell_Order (World, Item);
            end loop;

            for Item of Organics loop
               Ship.Update.Add_Buy_Order (World, Item, Ship.Hold_Quantity);
            end loop;

            for R of Resources loop
               Ship.Update.Add_Buy_Order (World, R, Ship.Hold_Quantity);
            end loop;

            for Item of Organics loop
               Ship.Update.Add_Sell_Order (Capital, Item);
            end loop;

            for R of Resources loop
               Ship.Update.Add_Sell_Order (Capital, R);
            end loop;

            declare
               Buy_Factor : Unit_Real := 0.4;
            begin
               for Item of Food loop
                  Ship.Update.Add_Buy_Order
                    (Capital, Item,
                     Quantities.Scale (Ship.Hold_Quantity, Buy_Factor));
                  Buy_Factor := Buy_Factor / 4.0;
               end loop;

               for Item of Clothing loop
                  Ship.Update.Add_Buy_Order
                    (Capital, Item,
                     Quantities.Scale (Ship.Hold_Quantity, Buy_Factor));
               end loop;
            end;
         end if;

         Ship.Update.Cycle_Orders (True);
      end Initial_Trade_Route;

   begin

      World.Update.Add_Ship (Defender);

      for I in 1 .. 20 loop
         declare
            Capital : constant Concorde.Worlds.World_Type :=
                        Concorde.Galaxy.Capital_World;
            Trader   : constant Concorde.Ships.Ship_Type :=
                         Concorde.Ships.Create.New_Ship
                           (Owner  => World.Owner,
                            Name   =>
                              World.Owner.Name & " Trader" & I'Img,
                            World  => Capital,
                            Design => "trader");
         begin
            Trader.Update.Set_Trade_Route
              (Capital, World);
            Capital.Update.Add_Ship (Trader);
            Trader.Log_Trade ("new trade ship");
         end;
      end loop;

   end Create_Initial_Ships;

   -----------------
   -- Find_System --
   -----------------

   function Find_System
     (Start  : Concorde.Systems.Star_System_Type;
      OK     : not null access
        function (System : Concorde.Systems.Star_System_Type)
      return Boolean)
      return Concorde.Systems.Star_System_Type
   is

      function Find
        (Start : Concorde.Systems.Star_System_Type)
         return Concorde.Systems.Star_System_Type;

      procedure Connect
        (System       : Concorde.Systems.Star_System_Type;
         Minimum      : Positive;
         Maximum      : Positive;
         Max_Distance : Non_Negative_Real);

      -------------
      -- Connect --
      -------------

      procedure Connect
        (System       : Concorde.Systems.Star_System_Type;
         Minimum      : Positive;
         Maximum      : Positive;
         Max_Distance : Non_Negative_Real)
      is
         List : Concorde.Systems.Lists.List;

         procedure Add_System (S : Concorde.Systems.Star_System_Type);

         ----------------
         -- Add_System --
         ----------------

         procedure Add_System (S : Concorde.Systems.Star_System_Type) is
            use type Concorde.Systems.Star_System_Type;
            use Concorde.Systems.Lists;
            D : constant Non_Negative_Real :=
                  Concorde.Systems.Distance (System, S);
            Position : Cursor := List.First;
         begin
            if S /= System
              and then not Concorde.Galaxy.Neighbours (System, S)
              and then (D <= Max_Distance
                        or else Natural (List.Length) < Minimum)
            then
               while Has_Element (Position)
                 and then Concorde.Systems.Distance
                   (Element (Position), System)
                     < D
               loop
                  Next (Position);
               end loop;
               if Has_Element (Position) then
                  List.Insert (Position, S);
               elsif Natural (List.Length) < Maximum then
                  List.Append (S);
               end if;
            end if;
         end Add_System;

      begin

         Concorde.Systems.Scan_Systems (Add_System'Access);

         for Target of List loop
            exit when Galaxy.Neighbours (System)'Length >= Maximum;
            Concorde.Galaxy.Connect (System.Index, Target.Index);
         end loop;

      end Connect;

      ----------
      -- Find --
      ----------

      function Find
        (Start : Concorde.Systems.Star_System_Type)
         return Concorde.Systems.Star_System_Type
      is
         package Tried_Vectors is
           new Memor.Element_Vectors
             (Concorde.Systems.Root_Star_System_Type, Boolean, False);

         Queue : Concorde.Systems.Lists.List;
         Tried : Tried_Vectors.Vector;
      begin
         Queue.Append (Start);
         while not Queue.Is_Empty loop
            declare
               System : constant Concorde.Systems.Star_System_Type :=
                          Queue.First_Element;
            begin
               Queue.Delete_First;
               if not Tried.Element (System) then
                  Tried.Replace_Element (System, True);

                  if OK (System) then
                     return System;
                  else
                     Connect (System, 2, 4, 0.1);
                     for N of Concorde.Galaxy.Neighbours (System) loop
                        if not Tried.Element (N) then
                           Queue.Append (N);
                        end if;
                     end loop;
                  end if;
               end if;
            end;
         end loop;
         return null;
      end Find;

   begin
      return Find (Start);
   end Find_System;

   ----------------
   -- New_Faction --
   ----------------

   function New_Faction
     (Name                : String;
      Capital             : String;
      Colour              : Lui.Colours.Colour_Type;
      Default_Ship_Design : String)
      return Faction_Type
   is

      Taken : Concorde.Galaxy.Star_System_Set;

      Start_World  : Concorde.Worlds.World_Type;
      Start_System : Concorde.Systems.Star_System_Type;

      procedure Add_Taken_Systems
        (Faction : Root_Faction_Type'Class);

      procedure Set_Initial_Prices
        (Market : Concorde.Markets.Updateable_Reference);

      procedure Create
        (New_Faction : in out Root_Faction_Type'Class);

      -----------------------
      -- Add_Taken_Systems --
      -----------------------

      procedure Add_Taken_Systems
        (Faction : Root_Faction_Type'Class)
      is
      begin
         Concorde.Galaxy.Add_Systems
           (Taken, Faction.Capital.System, 0.4);
      end Add_Taken_Systems;

      ------------
      -- Create --
      ------------

      procedure Create
        (New_Faction : in out Root_Faction_Type'Class)
      is

         function OK_For_Start
           (System : Concorde.Systems.Star_System_Type)
            return Boolean;

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

            System.Check_Loaded;

            declare
               use all type Concorde.Stars.Stellar_Class_Type;
               Class : constant Concorde.Stars.Stellar_Class_Type :=
                         Concorde.Stars.Star_Type
                           (System.Main_Object).Stellar_Class;
            begin
               case Class is
                  when F | G | K | M =>
                     null;
                  when O | B | A | L =>
                     return False;
               end case;
            end;

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
                 (System_Object : not null access constant
                    Systems.Star_System_Object_Interface'Class);

               -----------------
               -- Check_World --
               -----------------

               procedure Check_World
                 (System_Object : not null access constant
                    Systems.Star_System_Object_Interface'Class)
               is
                  use Concorde.Worlds;
               begin
                  if not Good_Starting_World
                    and then System_Object.all in Root_World_Type'Class
                  then
                     declare
                        W : constant World_Type :=
                              World_Type (System_Object);
                     begin
                        if W.Category = Terrestrial then
                           if W.Minimum_Temperature < 220.0 then
                              Ada.Text_IO.Put_Line
                                ("Rejecting " & W.Name &
                                   " because min temperature is "
                                 & Concorde.Real_Images.Approximate_Image
                                   (W.Minimum_Temperature - 273.0)
                                 & " degrees");
                           elsif W.Maximum_Temperature > 3730.0 then
                              Ada.Text_IO.Put_Line
                                ("Rejecting " & W.Name &
                                   " because max temperature is "
                                 & Concorde.Real_Images.Approximate_Image
                                   (W.Maximum_Temperature - 273.0)
                                 & " degrees");
                           elsif W.Hydrosphere not in 0.2 .. 0.75 then
                              Ada.Text_IO.Put_Line
                                ("Rejecting " & W.Name &
                                   " because hydrosphere is "
                                 & Concorde.Real_Images.Approximate_Image
                                   (W.Hydrosphere * 100.0)
                                 & "%");
                           else
                              Good_Starting_World := True;
                              Start_World := W;
                           end if;
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

            return True;

         end OK_For_Start;

      begin

         if not Concorde.Scenarios.Imperial_Centre then
            Imperial_Centre := False;
         end if;

         Start_System :=
           Find_System
             (Concorde.Galaxy.Get_System (1),
              OK_For_Start'Access);

         New_Faction.New_Agent
           (Concorde.Locations.Nowhere,
            null,
            Concorde.Quantities.Zero);

         New_Faction.Set_Cash (Concorde.Money.To_Money (1_000_000.0));
         New_Faction.Identifier :=
           Ada.Strings.Unbounded.To_Unbounded_String (Name);
         New_Faction.Set_Name (Name);
         New_Faction.System_Data :=
           new System_Data_Array (1 .. Galaxy.System_Count);
         New_Faction.Colour := Colour;
         New_Faction.Capital_World := Start_World;

         if Imperial_Centre then
            Galaxy.Set_Capital_World (Start_World);
         end if;

         New_Faction.Current_Systems := 1;
         New_Faction.Default_Ship := new String'(Default_Ship_Design);

         if False
           and then Concorde.Scenarios.Imperial_Centre
           and then not Concorde.Galaxy.Neighbours (1, Start_System.Index)
         then
            Concorde.Galaxy.Connect (1, Start_System.Index);
         end if;

         Imperial_Centre := False;

      end Create;

      ------------------------
      -- Set_Initial_Prices --
      ------------------------

      procedure Set_Initial_Prices
        (Market : Concorde.Markets.Updateable_Reference)
      is
         use Concorde.Commodities;
         Resources : constant Array_Of_Commodities :=
                       Concorde.Commodities.Get
                         (Concorde.Commodities.Resource);

      begin
         for Unavailable of Resources loop
            if Imperial_Centre then
               Market.Initial_Price
                 (Unavailable,
                  Concorde.Money.Adjust_Price
                    (Unavailable.Base_Price, Factor => 2.0));
               --           elsif Unavailable /= System.Resource then
               --              System.Market.Initial_Price
               --                (Unavailable,
               --                 Concorde.Money.Adjust_Price
               --             (Unavailable.Base_Price, Factor => 2.0));
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
      if False then
         Db.Scan (Add_Taken_Systems'Access);
      end if;

      declare
         Faction : constant Faction_Type :=
                    Db.Create (Create'Access);
      begin
         Start_System.Update.Set_Owner (Faction);
         Start_System.Update.Set_Capital (True);

         Start_World.Update.Set_Owner (Faction);
         Start_World.Update.Set_Capital (True);
         Start_World.Update.Set_Name (Capital);

         Start_World.Check_Loaded;

         if Imperial_Centre then
            Concorde.Colonies.Configure.Create_Colony_From_Template
              (Start_World, "imperial_capital");
         else
            Concorde.Colonies.Configure.Create_Colony_From_Template
              (Start_World, "initial");

            Create_Initial_Ships (Start_World);

         end if;

         if Start_World.Has_Market then
            Set_Initial_Prices (Start_World.Market.Update);
         else
            Ada.Text_IO.Put_Line
              ("Could not create initial colony on "
               & Start_World.Name);
         end if;

         Faction.Save_Agent;

         return Faction;

      end;

   end New_Faction;

end Concorde.Factions.Create;