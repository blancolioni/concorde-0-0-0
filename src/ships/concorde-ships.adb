with WL.Random;

with Concorde.Systems;

with Concorde.Money;
with Concorde.Random;

with Concorde.Empires.Db;
with Concorde.Modules.Db;
with Concorde.Ships.Db;
with Concorde.Worlds.Db;

with Concorde.Empires.Logging;

with Concorde.Markets;

package body Concorde.Ships is

   procedure Apply_Hit
     (Ship   : in out Root_Ship_Type'Class;
      Damage : Natural);

   procedure Calculate_Damage
     (Ship : in out Root_Ship_Type'Class);

   -------------------
   -- Add_Buy_Order --
   -------------------

   procedure Add_Buy_Order
     (Ship   : in out Root_Ship_Type'Class;
      World  : not null access constant
        Concorde.Worlds.Root_World_Type'Class;
      Item   : Concorde.Commodities.Commodity_Type)
   is
   begin
      Ship.Orders.Append
        ((Buy, World, Item));
   end Add_Buy_Order;

   --------------------
   -- Add_Sell_Order --
   --------------------

   procedure Add_Sell_Order
     (Ship   : in out Root_Ship_Type'Class;
      World  : not null access constant
        Concorde.Worlds.Root_World_Type'Class;
      Item   : Concorde.Commodities.Commodity_Type)
   is
   begin
      Ship.Orders.Append
        ((Sell, World, Item));
   end Add_Sell_Order;

   ----------------------
   -- Add_Trade_Offers --
   ----------------------

   overriding procedure Add_Trade_Offers
     (Ship   : not null access constant Root_Ship_Type)
   is
      use Concorde.Quantities;
      use type Memor.Database_Reference;
      Hold  : constant Quantity := Ship.Hold_Quantity;
      Space : constant Quantity := Hold - Ship.Total_Quantity;

      type Buy_Commodity is
         record
            Commodity : Concorde.Commodities.Commodity_Type;
            Supply    : Quantity;
            Demand    : Quantity;
            Buy_At    : Concorde.Money.Price_Type;
            Sell_At   : Concorde.Money.Price_Type;
            Score     : Natural;
         end record;

      package List_Of_Buy_Commodities is
        new Ada.Containers.Doubly_Linked_Lists (Buy_Commodity);

      Buy_List : List_Of_Buy_Commodities.List;

      function Matching_Sell_Order
        (Commodity : Concorde.Commodities.Commodity_Type)
         return Concorde.Worlds.World_Type;

      function Score_Trade
        (Supply, Demand  : Quantity;
         Buy_At, Sell_At : Concorde.Money.Price_Type)
         return Integer;

      -------------------------
      -- Matching_Sell_Order --
      -------------------------

      function Matching_Sell_Order
        (Commodity : Concorde.Commodities.Commodity_Type)
         return Concorde.Worlds.World_Type
      is
         use Concorde.Commodities, Concorde.Worlds;
      begin
         for Order of Ship.Orders loop
            if Order.Order = Sell
              and then Order.Commodity = Commodity
            then
               return World_Type (Order.World);
            end if;
         end loop;
         return null;
      end Matching_Sell_Order;

      -----------------
      -- Score_Trade --
      -----------------

      function Score_Trade
        (Supply, Demand  : Quantity;
         Buy_At, Sell_At : Concorde.Money.Price_Type)
         return Integer
      is
         use Concorde.Money;
      begin
         return To_Natural (Min (Supply, Demand))
           * Integer (100.0 * (To_Real (Sell_At) - To_Real (Buy_At)));
      end Score_Trade;

   begin

      for Current_Order of Ship.Orders loop
         exit when not Ship.Orbiting (Current_Order.World);

         case Current_Order.Order is
            when No_Order =>
               null;
            when Buy =>
               declare
                  use type Concorde.Worlds.World_Type;
                  Commodity : constant Concorde.Commodities.Commodity_Type :=
                                Current_Order.Commodity;
                  Destination : constant Concorde.Worlds.World_Type :=
                                  Matching_Sell_Order (Commodity);
               begin
                  if Destination /= null then
                     declare
                        From : constant Concorde.Markets.Market_Type :=
                                 Current_Order.World.Market;
                        To   : constant Concorde.Markets.Market_Type :=
                                 Destination.Market;
                        Supply : constant Quantity :=
                                   From.Current_Export_Supply
                                     (Commodity);
                        Demand : constant Quantity :=
                                   To.Current_Import_Demand (Commodity);
                        Buy_At : constant Concorde.Money.Price_Type :=
                                   From.Last_Average_Ask (Commodity);
                        Sell_At : constant Concorde.Money.Price_Type :=
                                    To.Last_Average_Ask (Commodity);
                        Score   : constant Integer :=
                                    Score_Trade
                                      (Supply, Demand, Buy_At, Sell_At);
                        Rec     : constant Buy_Commodity :=
                                    (Commodity => Current_Order.Commodity,
                                     Supply    => Supply,
                                     Demand    => Demand,
                                     Buy_At    => Buy_At,
                                     Sell_At   => Sell_At,
                                     Score     =>
                                       Integer'Max (Score, 0));
                     begin
                        Ship.Log_Trade
                          (Commodity.Name
                           & ": supply "
                           & Image (Supply)
                           & " @ "
                           & Money.Image (Buy_At)
                           & "; demand at "
                           & To.Name
                           & " is "
                           & Image (Demand)
                           & " @ "
                           & Money.Image (Sell_At)
                           & "; score "
                           & Score'Img);
                        if Score > 0 then
                           Buy_List.Append (Rec);
                        end if;
                     end;
                  else
                     Concorde.Agents.Create_Buy_Offer
                       (Agent     => Ship,
                        Commodity => Current_Order.Commodity,
                        Desired   => Space,
                        Minimum   => Space);
                  end if;
               end;
            when Sell =>
               declare
                  Available : constant Quantity :=
                                Ship.Get_Quantity (Current_Order.Commodity);
               begin
                  if Available > Zero then
                     Ship.Create_Sell_Offer
                       (Current_Order.Commodity, Available,
                        Ship.Get_Value (Current_Order.Commodity));
                  end if;
               end;
            when Colonise =>
               null;
         end case;
      end loop;

      declare
         Total_Score : Natural := 0;
      begin
         for Trade of Buy_List loop
            Total_Score := Total_Score + Trade.Score;
         end loop;

         for Trade of Buy_List loop
            Concorde.Agents.Create_Buy_Offer
              (Agent     => Ship,
               Commodity => Trade.Commodity,
               Desired   =>
                 Scale (Hold, Real (Trade.Score) / Real (Total_Score)),
               Minimum   =>
                 Scale (Hold, Real (Trade.Score) / Real (Total_Score)));
         end loop;
      end;
   end Add_Trade_Offers;

   -----------
   -- Alive --
   -----------

   function Alive
     (Ship : Root_Ship_Type'Class)
      return Boolean
   is
   begin
      return Ship.Alive;
   end Alive;

   ---------------
   -- Apply_Hit --
   ---------------

   procedure Apply_Hit
     (Ship   : in out Root_Ship_Type'Class;
      Damage : Natural)
   is
      Remaining : Non_Negative_Real := Non_Negative_Real (Damage);
   begin

      if not Ship.Alive then
         return;
      end if;

      for Mount of Ship.Structure loop
         if Mount.Module.Is_Shield then
            declare
               Shield : constant Non_Negative_Real :=
                          Mount.Module.Stored_Energy;
               Blocked : Non_Negative_Real;

               procedure Update
                 (Module : in out Concorde.Modules.Root_Module_Type'Class);

               ------------
               -- Update --
               ------------

               procedure Update
                 (Module : in out Concorde.Modules.Root_Module_Type'Class)
               is
               begin
                  Module.Execute (Blocked);
               end Update;

            begin
               if Remaining < Shield / 5.0 then
                  Blocked := Remaining;
               else
                  Blocked := Shield / 5.0;
               end if;

               Remaining := Remaining - Blocked;
               Concorde.Modules.Db.Update
                 (Mount.Module.Reference, Update'Access);
            end;
         end if;
      end loop;

      declare
         Mounts : constant Array_Of_Mounted_Modules :=
                    Ship.Get_Effective_Mounts;
         Index  : constant Positive :=
                    WL.Random.Random_Number
                      (Mounts'First, Mounts'Last);

         procedure Update
           (Module : in out Concorde.Modules.Root_Module_Type'Class);

         ------------
         -- Update --
         ------------

         procedure Update
           (Module : in out Concorde.Modules.Root_Module_Type'Class)
         is
         begin
            Module.Hit (Natural (Remaining));
            declare
               Chance : constant Unit_Real :=
                          Module.Explosion_Chance;
            begin
               if Concorde.Random.Unit_Random < Chance then
                  Module.Start_Explosion;
               end if;
            end;
         end Update;

      begin

         Concorde.Modules.Db.Update
           (Ship.Structure
              (Positive (Mounts (Index))).Module.Reference,
            Update'Access);
      end;

      Calculate_Damage (Ship);
      Ship.Alive := Ship.Current_Damage < 0.95;

      if not Ship.Alive then
         declare
            procedure Remove
              (Empire : in out Concorde.Empires.Root_Empire_Type'Class);

            ------------
            -- Remove --
            ------------

            procedure Remove
              (Empire : in out Concorde.Empires.Root_Empire_Type'Class)
            is
            begin
               Empire.Remove_Ship;
            end Remove;

         begin
            Concorde.Empires.Db.Update
              (Ship.Owner.Reference, Remove'Access);
         end;
      end if;
   end Apply_Hit;

   ----------------------
   -- Calculate_Damage --
   ----------------------

   procedure Calculate_Damage
     (Ship : in out Root_Ship_Type'Class)
   is
      use Concorde.Components;
      Total_Damage  : Non_Negative_Real := 0.0;
      Total_Shields : Non_Negative_Real := 0.0;
      Shield_Count  : Natural := 0;
      Module_Count  : Natural := 0;
   begin
      for Mount of Ship.Structure loop
         declare
            Module : constant Concorde.Modules.Module_Type :=
                       Mount.Module;
            Is_Strut : constant Boolean :=
                         Module.Component.Class = Strut;
            Is_Shield : constant Boolean :=
                          Module.Component.Class = Shield_Generator;
         begin
            if Is_Strut then
               --  don't include strut damage (for some reason?)
               null;
            else
               Total_Damage := Total_Damage + Module.Damage;
               Module_Count := Module_Count + 1;
               if Is_Shield then
                  Total_Shields := Total_Shields
                    + Module.Effectiveness * Module.Charge;
                  Shield_Count := Shield_Count + 1;
               end if;
            end if;
         end;
      end loop;

      if Module_Count > 0 then
         Ship.Current_Damage :=
           Total_Damage / Non_Negative_Real (Module_Count);
      else
         Ship.Current_Damage := 0.0;
      end if;
      if Shield_Count > 0 then
         Ship.Current_Shields := Total_Shields / Real (Shield_Count);
      else
         Ship.Current_Shields := 0.0;
      end if;
   end Calculate_Damage;

   -----------------------
   -- Clear_Destination --
   -----------------------

   procedure Clear_Destination
     (Ship   : in out Root_Ship_Type'Class)
   is
   begin
      Ship.Dest_Reference := Memor.Null_Database_Reference;
   end Clear_Destination;

   ------------------
   -- Clear_Orders --
   ------------------

   procedure Clear_Orders
     (Ship : in out Root_Ship_Type'Class)
   is
   begin
      Ship.Orders.Clear;
   end Clear_Orders;

   -----------------
   -- Count_Ships --
   -----------------

   function Count_Ships
     (Test : not null access function
        (Ship : Ship_Type)
      return Boolean)
      return Natural
   is
      Result : Natural := 0;

      procedure Process (Ship : Ship_Type);

      -------------
      -- Process --
      -------------

      procedure Process (Ship : Ship_Type) is
      begin
         if Test (Ship) then
            Result := Result + 1;
         end if;
      end Process;

   begin
      Db.Scan (Process'Access);
      return Result;
   end Count_Ships;

   ------------------
   -- Current_Mass --
   ------------------

   function Current_Mass
     (Ship : Root_Ship_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Ship.Empty_Mass + Ship.Total_Mass;
   end Current_Mass;

   ------------------
   -- Cycle_Orders --
   ------------------

   procedure Cycle_Orders
     (Ship  : in out Root_Ship_Type'Class;
      Cycle : Boolean)
   is
   begin
      Ship.Cycle_Orders := Cycle;
   end Cycle_Orders;

   ------------
   -- Damage --
   ------------

   function Damage
     (Ship : Root_Ship_Type'Class)
      return Unit_Real
   is
   begin
      return Ship.Current_Damage;
   end Damage;

   -----------------
   -- Destination --
   -----------------

   function Destination
     (Ship : Root_Ship_Type'Class)
      return access constant Concorde.Worlds.Root_World_Type'Class
   is
   begin
      return Concorde.Worlds.Db.Reference (Ship.Dest_Reference);
   end Destination;

   ----------------
   -- Empty_Mass --
   ----------------

   function Empty_Mass
     (Ship : Root_Ship_Type'Class)
      return Non_Negative_Real
   is
      Result : Non_Negative_Real := 0.0;
   begin
      for Mount of Ship.Structure loop
         Result := Result + Mount.Module.Mass;
      end loop;
      return Result;
   end Empty_Mass;

   ----------------------------
   -- Execute_Arrival_Orders --
   ----------------------------

   procedure Execute_Arrival_Orders
     (Ship : in out Root_Ship_Type'Class)
   is
      pragma Unreferenced (Ship);
   begin
      null;
   end Execute_Arrival_Orders;

   ----------------------
   -- Get_Class_Mounts --
   ----------------------

   function Get_Class_Mounts
     (Ship  : Root_Ship_Type'Class;
      Class : Concorde.Components.Component_Class)
      return Array_Of_Mounted_Modules
   is
      use type Concorde.Components.Component_Class;
      function Is_Of_Class (Module : Concorde.Modules.Module_Type)
                            return Boolean
      is (Module.Component.Class = Class);
   begin
      return Ship.Get_Matching_Mounts (Is_Of_Class'Access);
   end Get_Class_Mounts;

   ------------------------
   -- Get_Damaged_Mounts --
   ------------------------

   function Get_Damaged_Mounts
     (Ship : Root_Ship_Type'Class)
      return Array_Of_Mounted_Modules
   is
      use Concorde.Components;
      Result : Array_Of_Mounted_Modules
        (1 .. Natural (Ship.Structure.Last_Index));
      Count  : Natural := 0;
   begin
      for I in 1 .. Ship.Structure.Last_Index loop
         if Ship.Structure (I).Module.Damage > 0.0 then
            Count := Count + 1;
            Result (Count) := Mounted_Module (I);
         end if;
      end loop;
      return Result (1 .. Count);
   end Get_Damaged_Mounts;

   --------------------------
   -- Get_Effective_Mounts --
   --------------------------

   function Get_Effective_Mounts
     (Ship : Root_Ship_Type'Class)
      return Array_Of_Mounted_Modules
   is
      use Concorde.Components;
      Result : Array_Of_Mounted_Modules
        (1 .. Natural (Ship.Structure.Last_Index));
      Count  : Natural := 0;
   begin
      for I in 1 .. Ship.Structure.Last_Index loop
         if Ship.Structure (I).Module.Damage < 1.0 then
            Count := Count + 1;
            Result (Count) := Mounted_Module (I);
         end if;
      end loop;
      return Result (1 .. Count);
   end Get_Effective_Mounts;

   -------------------------
   -- Get_Matching_Mounts --
   -------------------------

   function Get_Matching_Mounts
     (Ship  : Root_Ship_Type'Class;
      Match : not null access
        function (Module : Concorde.Modules.Module_Type)
      return Boolean)
      return Array_Of_Mounted_Modules
   is
      use Concorde.Components;
      Result : Array_Of_Mounted_Modules
        (1 .. Natural (Ship.Structure.Last_Index));
      Count  : Natural := 0;
   begin
      for I in 1 .. Ship.Structure.Last_Index loop
         if Match (Ship.Structure.Element (I).Module) then
            Count := Count + 1;
            Result (Count) := Mounted_Module (I);
         end if;
      end loop;
      return Result (1 .. Count);
   end Get_Matching_Mounts;

   ----------------
   -- Get_Module --
   ----------------

   function Get_Module
     (Ship  : Root_Ship_Type'Class;
      Mount : Mounted_Module)
      return Concorde.Modules.Module_Type
   is
   begin
      return Ship.Structure (Positive (Mount)).Module;
   end Get_Module;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
     (Ship  : Root_Ship_Type'Class;
      Mount : Mounted_Module)
      return Module_Orientation
   is
   begin
      return Ship.Structure (Positive (Mount)).Orientation;
   end Get_Orientation;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position
     (Ship  : Root_Ship_Type'Class;
      Mount : Mounted_Module)
      return Module_Position
   is
   begin
      return Ship.Structure (Positive (Mount)).Left_Low_Aft;
   end Get_Position;

   -----------------------
   -- Get_Weapon_Mounts --
   -----------------------

   function Get_Weapon_Mounts
     (Ship : Root_Ship_Type'Class)
      return Array_Of_Mounted_Modules
   is
      use Concorde.Components;
      Result : Array_Of_Mounted_Modules
        (1 .. Natural (Ship.Structure.Last_Index));
      Count  : Natural := 0;
   begin
      for I in 1 .. Ship.Structure.Last_Index loop
         if Ship.Structure (I).Module.Component.Class in Weapon_Class then
            Count := Count + 1;
            Result (Count) := Mounted_Module (I);
         end if;
      end loop;
      return Result (1 .. Count);
   end Get_Weapon_Mounts;

   ----------------------------
   -- Has_Colonisation_Order --
   ----------------------------

   function Has_Colonisation_Order
     (Ship : Root_Ship_Type'Class)
      return Boolean
   is
   begin
      for Order of Ship.Orders loop
         if Order.Order = Colonise then
            return True;
         end if;
      end loop;
      return False;
   end Has_Colonisation_Order;

   ---------------------
   -- Has_Destination --
   ---------------------

   function Has_Destination
     (Ship : Root_Ship_Type'Class)
      return Boolean
   is
      use type Memor.Database_Reference;
   begin
      return Ship.Dest_Reference /= Memor.Null_Database_Reference;
   end Has_Destination;

   --------------------------
   -- Has_Effective_Engine --
   --------------------------

   function Has_Effective_Engine
     (Ship : Root_Ship_Type'Class)
      return Boolean
   is
      use Concorde.Components;
   begin
      for Mount of Ship.Structure loop
         if Mount.Module.Effectiveness > 0.0
           and then Mount.Module.Component.Class = Drive
         then
            return True;
         end if;
      end loop;
      return False;
   end Has_Effective_Engine;

   --------------------------
   -- Has_Effective_Weapon --
   --------------------------

   function Has_Effective_Weapon
     (Ship : Root_Ship_Type'Class)
      return Boolean
   is
      use Concorde.Components;
   begin
      for Mount of Ship.Structure loop
         if Mount.Module.Effectiveness > 0.0
           and then Mount.Module.Component.Class in Weapon_Class
         then
            return True;
         end if;
      end loop;
      return False;
   end Has_Effective_Weapon;

   ----------------
   -- Has_Orders --
   ----------------

   function Has_Orders
     (Ship : Root_Ship_Type'Class)
      return Boolean
   is
   begin
      return not Ship.Orders.Is_Empty;
   end Has_Orders;

   ---------
   -- Hit --
   ---------

   procedure Hit
     (Target : in out Root_Ship_Type'Class;
      Damage : Natural)
   is
   begin
      Apply_Hit (Target, Damage);
   end Hit;

   ---------------
   -- Hold_Size --
   ---------------

   function Hold_Size
     (Ship : Root_Ship_Type'Class)
      return Non_Negative_Real
   is
      Holds  : constant Array_Of_Mounted_Modules :=
                 Ship.Get_Class_Mounts (Concorde.Components.Hold);
      Result : Non_Negative_Real := 0.0;
   begin
      for Hold of Holds loop
         Result := Result + Real (Ship.Get_Module (Hold).Volume);
      end loop;
      return Result;
   end Hold_Size;

   ---------------
   -- Long_Name --
   ---------------

   function Long_Name (Ship : Root_Ship_Type'Class) return String is
   begin
      return Ship.Identity & " " & Ship.Name & " ["
        & Concorde.Locations.Short_Name (Ship.Current_Location) & "]";
   end Long_Name;

   --------------------
   -- Maximum_Thrust --
   --------------------

   function Maximum_Thrust
     (Ship : Root_Ship_Type'Class)
      return Non_Negative_Real
   is
      Drives : constant Array_Of_Mounted_Modules :=
                 Ship.Get_Drive_Mounts;
      Result : Non_Negative_Real := 0.0;
   begin
      for Mount_Index of Drives loop
         declare
            Mount : Module_Layout_Record renames
                      Ship.Structure (Positive (Mount_Index));
         begin
            if Mount.Orientation.Axis = Z_Axis
              and then Mount.Orientation.Forward
            then
               Result := Result + Mount.Module.Maximum_Output;
            end if;
         end;
      end loop;
      return Result;
   end Maximum_Thrust;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Ship : Root_Ship_Type)
      return Memor.Root_Database_Type'Class
   is
      pragma Unreferenced (Ship);
   begin
      return Db.Get_Database;
   end Object_Database;

   -----------
   -- Owner --
   -----------

   function Owner
     (Ship : Root_Ship_Type'Class)
      return access constant Concorde.Empires.Root_Empire_Type'Class
   is
   begin
      return Ship.Owner;
   end Owner;

   ------------
   -- Repair --
   ------------

   procedure Repair
     (Ship   : in out Root_Ship_Type'Class;
      Points : Positive)
   is
      Mounts : constant Array_Of_Mounted_Modules :=
                 Ship.Get_Damaged_Mounts;

      Remaining : Natural := Points;

      procedure Update
        (Module : in out Concorde.Modules.Root_Module_Type'Class);

      ------------
      -- Update --
      ------------

      procedure Update
        (Module : in out Concorde.Modules.Root_Module_Type'Class)
      is
      begin
         Concorde.Empires.Logging.Log
           (Ship.Owner,
            Ship.Short_Description
            & ": repairing" & Remaining'Img
            & " damage from " & Module.Name
            & " with damage "
            & Lui.Approximate_Image (Module.Damage));
         Module.Repair (Remaining);
      end Update;

   begin
      for Mount of Mounts loop
         Concorde.Modules.Db.Update
           (Ref     => Ship.Structure (Positive (Mount)).Module.Reference,
            Updater => Update'Access);
         exit when Remaining = 0;
      end loop;
      Calculate_Damage (Ship);
   end Repair;

   ----------------------------
   -- Set_Colonisation_Order --
   ----------------------------

   procedure Set_Colonisation_Order
     (Ship : in out Root_Ship_Type'Class)
   is
   begin
      Ship.Orders.Append
        ((Colonise, Concorde.Worlds.World_Type (Ship.Orbiting), null));
   end Set_Colonisation_Order;

   ---------------------
   -- Set_Destination --
   ---------------------

   procedure Set_Destination
     (Ship   : in out Root_Ship_Type'Class;
      World : not null access constant
        Concorde.Worlds.Root_World_Type'Class)
   is
   begin
      Ship.Dest_System_Reference := World.System.Reference;
      Ship.Dest_Reference := World.Reference;
   end Set_Destination;

   ---------------------
   -- Set_Destination --
   ---------------------

   procedure Set_Destination
     (Ship    : in out Root_Ship_Type'Class;
      System  : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
   is
   begin
      Ship.Dest_Reference := Memor.Null_Database_Reference;
      Ship.Dest_System_Reference := System.Reference;
   end Set_Destination;

   --------------
   -- Set_Name --
   --------------

   overriding procedure Set_Name
     (Ship : in out Root_Ship_Type;
      Name : String)
   is
   begin
      Ship.Ship_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Set_Name;

   ---------------
   -- Set_Owner --
   ---------------

   procedure Set_Owner
     (Ship      : in out Root_Ship_Type'Class;
      New_Owner : not null access constant
        Concorde.Empires.Root_Empire_Type'Class)
   is
   begin
      Ship.Owner := New_Owner;
   end Set_Owner;

   -------------
   -- Shields --
   -------------

   function Shields
     (Ship : Root_Ship_Type'Class)
      return Unit_Real
   is
   begin
      return Ship.Current_Shields;
   end Shields;

   -----------------------
   -- Short_Description --
   -----------------------

   function Short_Description (Ship : Root_Ship_Type'Class) return String is
   begin
      return Ship.Long_Name & " dmg"
        & Natural'Image (Natural (Ship.Damage * 100.0)) & "%";
   end Short_Description;

   ----------
   -- Size --
   ----------

   function Size
     (Ship : Root_Ship_Type'Class)
      return Size_Type
   is
   begin
      return Ship.Size;
   end Size;

   ------------------------
   -- Standard_Full_Mass --
   ------------------------

   function Standard_Full_Mass
     (Ship : Root_Ship_Type'Class)
      return Non_Negative_Real
   is
      Holds : constant Array_Of_Mounted_Modules :=
                Ship.Get_Class_Mounts (Concorde.Components.Hold);
      Tanks : constant Array_Of_Mounted_Modules :=
                Ship.Get_Class_Mounts (Concorde.Components.Hold);
      Result : Non_Negative_Real := Ship.Empty_Mass;
   begin
      --  assume tanks and holds full of water (1_000 kg / m3)
      for Hold_Index of Holds loop
         Result := Result
           + 1000.0 * Real (Ship.Get_Module (Hold_Index).Volume);
      end loop;

      for Tank_Index of Tanks loop
         Result := Result
           + 1000.0 * Real (Ship.Get_Module (Tank_Index).Volume);
      end loop;
      return Result;
   end Standard_Full_Mass;

   ---------------
   -- Tank_Size --
   ---------------

   function Tank_Size
     (Ship : Root_Ship_Type'Class)
      return Non_Negative_Real
   is
      Tanks : constant Array_Of_Mounted_Modules :=
                Ship.Get_Class_Mounts (Concorde.Components.Tank);
      Result : Non_Negative_Real := 0.0;
   begin
      for Tank of Tanks loop
         Result := Result + Real (Ship.Get_Module (Tank).Volume);
      end loop;
      return Result;
   end Tank_Size;

   -------------------
   -- Update_Damage --
   -------------------

   procedure Update_Damage
     (Ship : Root_Ship_Type'Class)
   is
      procedure Update (Ship : in out Root_Ship_Type'Class);

      ------------
      -- Update --
      ------------

      procedure Update (Ship : in out Root_Ship_Type'Class) is
         New_Hits : Natural := 0;
      begin
         for Mount of Ship.Structure loop
            declare
               procedure Update_Module
                 (Module : in out Concorde.Modules.Root_Module_Type'Class);

               -------------------
               -- Update_Module --
               -------------------

               procedure Update_Module
                 (Module : in out Concorde.Modules.Root_Module_Type'Class)
               is
               begin
                  Module.Update_Damage;
                  if Module.Exploding then
                     if Module.Explosion_Timer = 0 then
                        New_Hits := New_Hits + Module.Explosion_Size;
                     end if;
                  end if;
               end Update_Module;

            begin
               Concorde.Modules.Db.Update
                 (Mount.Module.Reference, Update_Module'Access);
            end;
         end loop;

         if New_Hits > 0 then
            Ship.Hit (New_Hits);
         end if;

      end Update;

   begin
      Db.Update (Ship.Reference, Update'Access);
   end Update_Damage;

   ------------------
   -- Update_Power --
   ------------------

   procedure Update_Power
     (Ship : Root_Ship_Type'Class)
   is
      procedure Update (Ship : in out Root_Ship_Type'Class);

      ------------
      -- Update --
      ------------

      procedure Update (Ship : in out Root_Ship_Type'Class) is
      begin
         for Mount of Ship.Structure loop
            declare
               procedure Update_Module
                 (Module : in out Concorde.Modules.Root_Module_Type'Class);

               -------------------
               -- Update_Module --
               -------------------

               procedure Update_Module
                 (Module : in out Concorde.Modules.Root_Module_Type'Class)
               is
               begin
                  Module.Draw_Power
                    (Module.Maximum_Power_Draw);
               end Update_Module;

            begin
               Concorde.Modules.Db.Update
                 (Mount.Module.Reference, Update_Module'Access);
            end;

         end loop;

         Calculate_Damage (Ship);

      end Update;

   begin
      Db.Update (Ship.Reference, Update'Access);
   end Update_Power;

end Concorde.Ships;
