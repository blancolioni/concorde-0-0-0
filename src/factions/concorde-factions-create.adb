with Ada.Text_IO;

with Concorde.Money;
with Concorde.Quantities;

with Concorde.Options;
with Concorde.Random;
with Concorde.Real_Images;

with Concorde.Calendar;
with Concorde.Objects.Queues;

with Concorde.Armies;
with Concorde.Galaxy;
with Concorde.Locations;
with Concorde.Markets;
with Concorde.Ships.Vessels.Create;
with Concorde.Stars;
with Concorde.Worlds;
with Concorde.Systems;

with Concorde.People.Communities.Create;
with Concorde.People.Groups;
with Concorde.People.Individuals.Create;
with Concorde.People.Individuals.Work;
with Concorde.People.Pops;

with Concorde.Scenarios;

--  with Concorde.Colonies.Configure;

with Concorde.Commodities;

with Concorde.Systems.Lists;

with Concorde.Ministries.Create;

with Concorde.Managers.Factions;
with Concorde.Managers.Ships.Trade;

package body Concorde.Factions.Create is

   Imperial_Centre : Boolean := True;

   procedure Create_Initial_Ships
     (Community : Concorde.People.Communities.Community_Type);

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
     (Community : Concorde.People.Communities.Community_Type)
   is
      World : constant Concorde.Worlds.World_Type :=
                Community.World;
      Initial_Trade_Ships : constant Natural :=
                              Concorde.Options.Initial_Trade_Ships;
      Activation_Range    : constant Duration :=
                              Concorde.Calendar.Days
                                (Initial_Trade_Ships);
      Defender            : constant Concorde.Ships.Ship_Type :=
                   Concorde.Ships.Vessels.Create.Create_Start_Vessel
                     (Owner       => Community.Owner,
                      Community   => Community,
                      Name        =>
                        Community.Owner.Name
                      & " Defender",
                      Design_Name => "defender");

   begin

      World.Update.Add_Ship (Defender);

      for I in 1 .. Initial_Trade_Ships loop
         declare
            Capital : constant Concorde.Worlds.World_Type :=
                        Concorde.Galaxy.Capital_World;
            Faction : constant Concorde.Factions.Faction_Type :=
                        Community.Owner;
            Trader   : constant Concorde.Ships.Ship_Type :=
                         Concorde.Ships.Vessels.Create.Create_Start_Vessel
                           (Owner       => Faction,
                            Name        =>
                              Faction.Name & " Trader" & I'Img,
                            Community   => Faction.Capital_Community,
                            Design_Name => "trader");
         begin

            declare
               use Concorde.Managers.Ships.Trade;
               Manager  : constant Ship_Trade_Manager :=
                            Create_Manager (Trader, Community);
            begin
               Manager.Activate;
            end;

            if I mod 2 = 1 then
               World.Update.Add_Ship (Trader);
            else
               Capital.Update.Add_Ship (Trader);
            end if;

            declare
               use Concorde.Calendar;
            begin
               Concorde.Objects.Queues.Next_Event
                 (Trader,
                  Concorde.Calendar.Clock
                  + Duration
                    (Concorde.Random.Unit_Random
                     * Real (Activation_Range)));
            end;

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
                     Connect (System, 2, 4, 0.25);
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
      Color               : Lui.Colors.Color_Type;
      Default_Ship_Design : String;
      Template            : Tropos.Configuration)
      return Faction_Type
   is
      pragma Unreferenced (Capital);

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
           (Taken, Faction.Capital_World.System, 0.4);
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

            for S of Ns loop
               if S.Owner /= null then
                  return False;
               end if;
            end loop;

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
                     Ada.Text_IO.Put_Line
                       (System.Main_Object.Name
                        & ": rejected because stellar class is "
                        & Class'Img);
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
                              null;
--                                New_Faction.Log
--                                  ("setup",
--                                   "Rejecting " & W.Name &
--                                     " because min temperature is "
--                                   & Concorde.Real_Images.Approximate_Image
--                                     (W.Minimum_Temperature - 273.0)
--                                   & " degrees");
                           elsif W.Maximum_Temperature > 373.0 then
                              null;
--                                New_Faction.Log
--                                  ("setup",
--                                   "Rejecting " & W.Name &
--                                     " because max temperature is "
--                                   & Concorde.Real_Images.Approximate_Image
--                                     (W.Maximum_Temperature - 273.0)
--                                   & " degrees");
                           elsif W.Hydrosphere not in 0.2 .. 0.75 then
                              Ada.Text_IO.Put_Line
                                (W.Name &
                                   ": rejected because hydrosphere is "
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
                  Ada.Text_IO.Put_Line
                    (System.Main_Object.Name
                     & ": rejected because no good starting worlds");
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
           (Location       =>
              Concorde.Locations.World_Surface (Start_World, 1),
            Government     => null,
            Market         => null,
            Cash           => Concorde.Money.To_Money (1_000_000.0),
            Stock_Capacity => Concorde.Quantities.Zero);

         New_Faction.Identifier :=
           Ada.Strings.Unbounded.To_Unbounded_String (Name);
         New_Faction.Set_Name (Name);
         New_Faction.System_Data :=
           new System_Data_Array (1 .. Galaxy.System_Count);
         New_Faction.Color := Color;

         if Imperial_Centre then
            Galaxy.Set_Capital_World (Start_World);
            New_Faction.Central_Bank := True;
         end if;

         New_Faction.Current_Systems := 1;
         New_Faction.Default_Ship := new String'(Default_Ship_Design);

         if False
           and then Concorde.Scenarios.Imperial_Centre
           and then not Concorde.Galaxy.Neighbours (1, Start_System.Index)
         then
            Concorde.Galaxy.Connect (1, Start_System.Index);
         end if;

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
                  Market.Initial_Price
                    (Item,
                     Concorde.Money.Adjust_Price
                       (Item.Base_Price, Factor => 2.0));
               else
                  Market.Initial_Price
                    (Item,
                     Concorde.Money.Adjust_Price
                       (Item.Base_Price, Factor => 4.0));
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
         Capital : constant Concorde.People.Communities.Community_Type :=
                     Concorde.People.Communities.Create.New_Community
                       (World      => Start_World,
                        Faction    => Faction,
                        Template   => Template);
      begin
         Start_System.Update.Set_Owner (Faction);
         Start_System.Update.Set_Capital (True);

         Faction.Update.Capital_Community := Capital;
         Capital.Update.Set_Owner (Faction);

         Start_World.Check_Loaded;

--           if Imperial_Centre then
--              Concorde.Colonies.Configure.Create_Colony_From_Template
--                (Start_World, "imperial_capital");
--           else
--              Concorde.Colonies.Configure.Create_Colony_From_Template
--                (Start_World, "initial_colony");
--
--              Create_Initial_Ships (Start_World);
--
--           end if;

         declare
            use Concorde.People.Individuals;
            House_Ministry  : constant Concorde.Ministries.Ministry_Type :=
                                Concorde.Ministries.Create
                                  .Create_Faction_Ministry (Faction);
            Ancestor        : constant Individual_Type :=
                         Concorde.People.Individuals.Create.Create_Family_Tree
                                  (Faction,
                                   Concorde.Locations.In_Community
                                     (Faction.Capital_Community));
         begin
            House_Ministry.Update.Set_Minister (Ancestor);
            Ancestor.Update.Set_Office (House_Ministry);
         end;

         Set_Initial_Prices
           (Faction.Capital_Community.Market.Update);

         Create_Initial_Ships
           (Concorde.People.Communities.Community_Type
              (Faction.Capital_Community));

--           if Start_World.Has_Market then
--              Set_Initial_Prices (Start_World.Market.Update);
--           else
--              Ada.Text_IO.Put_Line
--                ("Could not create initial colony on "
--                 & Start_World.Name);
--           end if;

         Faction.Save_Agent;
         Concorde.Managers.Factions.Create_Manager (Faction).Activate;

         declare
            procedure Add_Work (Pop : Concorde.People.Pops.Pop_Type);

            --------------
            -- Add_Work --
            --------------

            procedure Add_Work (Pop : Concorde.People.Pops.Pop_Type) is
               use Concorde.People.Groups;
            begin
               if Pop.Group.Is_State_Employee then
                  declare
                     Base_Wage    : constant Non_Negative_Real :=
                                      (case Pop.Group.Wealth is
                                          when Poor         => 1.0,
                                          when Middle_Class => 5.0,
                                          when Rich         => 25.0);
                  begin
                     Faction.Manager.Add_Work_Item
                       (Concorde.People.Individuals.Work.Pay_State_Employee
                          (Pop,
                           Concorde.Money.To_Price (Base_Wage)));
                  end;
               end if;
            end Add_Work;

         begin
            Capital.Scan_Pops (Add_Work'Access);
         end;

         declare
            procedure Add_Work (Ship : Concorde.Ships.Ship_Type);

            --------------
            -- Add_Work --
            --------------

            procedure Add_Work (Ship : Concorde.Ships.Ship_Type) is
               use Concorde.Ships, Concorde.People.Individuals.Work;
            begin
               if Ship.Owned_By (Faction) then
                  case Ship.Classification is
                     when Civilian =>
                        Faction.Manager.Add_Work_Item
                          (Appoint_Trader_Captain (Ship));
                     when Military =>
                        null;
                  end case;
               end if;
            end Add_Work;

         begin
            Faction.Capital_World.Scan_Ships (Add_Work'Access);
         end;

         declare
            use Concorde.Calendar;
         begin
            Concorde.Objects.Queues.Next_Event
              (Faction,
               Concorde.Calendar.Clock
               + Duration (Concorde.Random.Unit_Random * 86_400.0));
         end;

         Imperial_Centre := False;

         return Faction;

      end;

   end New_Faction;

end Concorde.Factions.Create;
