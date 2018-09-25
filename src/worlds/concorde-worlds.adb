--  with WL.String_Maps;

with Concorde.Factions;
with Concorde.Facilities;

with Concorde.Worlds.Create;
with Concorde.Worlds.Fields;

package body Concorde.Worlds is

   --------------
   -- Add_Ship --
   --------------

   procedure Add_Ship
     (World : in out Root_World_Type'Class;
      Ship  : not null access constant
        Concorde.Ships.Root_Ship_Type'Class)
   is
   begin
      World.Ships.Append
        (Concorde.Ships.Ship_Type (Ship));
   end Add_Ship;

   ---------
   -- Buy --
   ---------

--     procedure Buy
--       (World     : in out Root_World_Type'Class;
--        Commodity : Concorde.Commodities.Commodity_Type;
--        Quantity  : in out Concorde.Quantities.Quantity_Type)
--     is
--        use Concorde.Money, Concorde.Quantities;
--        New_Quantity : constant Quantity_Type :=
--                    Min (Quantity, World.Import_Market_Size (Commodity));
--        Cash : constant Money_Type :=
--                         Total (World.Buy_Price (Commodity), New_Quantity);
--
--     begin
--
--        declare
--           Upd : constant Concorde.Installations.Updateable_Reference :=
--                   World.Port.Update;
--        begin
--           Upd.Remove_Cash (Cash);
--           Upd.Add_Quantity (Commodity, New_Quantity, Cash);
--
--        end;
--
--        Quantity := New_Quantity;
--     end Buy;

   ---------------
   -- Buy_Price --
   ---------------

--     function Buy_Price
--       (World     : Root_World_Type'Class;
--        Commodity : Concorde.Commodities.Commodity_Type)
--        return Concorde.Money.Price_Type
--     is
--     begin
--        return Concorde.Money.Adjust_Price
--          (Concorde.Money.Without_Tax
--             (World.Market.Current_Price (Commodity),
--              Float
--                (World.Government.Tax_Rate
--                     (Concorde.Trades.Import, Commodity))),
--           0.9);
--     end Buy_Price;

   --------------
   -- Category --
   --------------

   function Category
     (World : Root_World_Type'Class)
      return World_Category
   is
   begin
      return World.Category;
   end Category;

   ------------------------
   -- Current_Local_Time --
   ------------------------

   function Current_Local_Time
     (World : Root_World_Type'Class)
      return Concorde.Geometry.Radians
   is
      use Concorde.Calendar;
      Cycle_Count   : Natural;
      Partial_Cycle : Duration;
   begin
      Split (Clock, Duration (World.Day_Length), Cycle_Count, Partial_Cycle);
      return Concorde.Geometry.Degrees_To_Radians
        (360.0 * Non_Negative_Real (Partial_Cycle) / World.Day_Length);
   end Current_Local_Time;

   ----------------
   -- Day_Length --
   ----------------

   function Day_Length
     (World : Root_World_Type'Class)
      return Non_Negative_Real
   is
   begin
      return World.Day_Length;
   end Day_Length;

   ------------------------
   -- Export_Market_Size --
   ------------------------

--     function Export_Market_Size
--       (World     : Root_World_Type'Class;
--        Commodity : Concorde.Commodities.Commodity_Type)
--        return Concorde.Quantities.Quantity_Type
--     is
--     begin
--        return World.Port.Get_Quantity (Commodity);
--     end Export_Market_Size;

   ---------------------
   -- Get_Field_Value --
   ---------------------

   overriding function Get_Field_Value
     (World : Root_World_Type;
      Name  : String)
      return Concorde.Network.Expression_Value
   is
   begin
      if not Concorde.Worlds.Fields.Have_Field (Name) then
         raise Constraint_Error with
           "worlds: no such field: " & Name;
      end if;
      return Concorde.Worlds.Fields.Get_Field (World, Name);
   end Get_Field_Value;

   -------------------------------
   -- Get_Sector_Infrastructure --
   -------------------------------

   function Get_Sector_Infrastructure
     (Location : Concorde.Locations.Sector_Location)
      return Unit_Real
   is
      World : constant World_Type :=
                World_Type (Concorde.Locations.Primary (Location));
      Tile  : constant Concorde.Surfaces.Surface_Tile_Index :=
                Concorde.Surfaces.Surface_Tile_Index
                  (Concorde.Locations.World_Sector (Location));
   begin
      return World.Sectors (Tile).Infrastructure;
   end Get_Sector_Infrastructure;

   -------------------------
   -- Get_Sector_Resource --
   -------------------------

   procedure Get_Sector_Resource
     (Location      : Concorde.Locations.Object_Location;
      Resource      : out Concorde.Commodities.Commodity_Type;
      Concentration : out Unit_Real;
      Accessibility : out Unit_Real)
   is
      World : constant World_Type :=
                World_Type (Concorde.Locations.Primary (Location));
      Tile  : constant Concorde.Surfaces.Surface_Tile_Index :=
                Concorde.Surfaces.Surface_Tile_Index
                  (Concorde.Locations.World_Sector (Location));
   begin
      World.Get_Sector_Resource (Tile, Resource, Concentration, Accessibility);
   end Get_Sector_Resource;

   -------------------------
   -- Get_Sector_Resource --
   -------------------------

   procedure Get_Sector_Resource
     (World         : Root_World_Type'Class;
      Sector        : Concorde.Surfaces.Surface_Tile_Index;
      Resource      : out Concorde.Commodities.Commodity_Type;
      Concentration : out Unit_Real;
      Accessibility : out Unit_Real)
   is
      Deposit : Deposit_Record renames
                  World.Sectors (Sector).Deposit;
   begin
      Resource := Deposit.Resource;
      Concentration := Deposit.Concentration;
      Accessibility := Deposit.Accessibility;
   end Get_Sector_Resource;

   ---------------
   -- Get_Ships --
   ---------------

   procedure Get_Ships
     (World : Root_World_Type'Class;
      Ships : out Concorde.Ships.Lists.List)
   is
   begin
      Ships.Clear;
      for Ship of World.Ships loop
         Ships.Append (Ship);
      end loop;
   end Get_Ships;

   ---------------
   -- Get_Value --
   ---------------

   overriding function Get_Value
     (World : Root_World_Type)
      return Concorde.Network.Expression_Value
   is
   begin
      return Concorde.Network.To_Expression_Value (World.Radius);
   end Get_Value;

   ---------------
   -- Grow_Food --
   ---------------

   overriding procedure Grow_Food
     (World           : in out Root_World_Type;
      Food_Production : Non_Negative_Real;
      Grown           : out Non_Negative_Real)
   is
   begin
      Grown := Food_Production * World.Habitability;
   end Grow_Food;

   ------------------
   -- Habitability --
   ------------------

   function Habitability
     (World : Root_World_Type'Class)
      return Unit_Real
   is
   begin
      return Unit_Clamp
        (abs (10.0 - (World.Surface_Temp - 288.0)) / 10.0);
   end Habitability;

   ---------------
   -- Has_Field --
   ---------------

   overriding function Has_Field
     (World : Root_World_Type;
      Name  : String)
      return Boolean
   is
      pragma Unreferenced (World);
   begin
      return Concorde.Worlds.Fields.Have_Field (Name);
   end Has_Field;

   --------------------
   -- Has_Government --
   --------------------

--     function Has_Government
--       (World : Root_World_Type'Class)
--        return Boolean
--     is
--        use type Concorde.Government.Government_Type;
--     begin
--        return World.Government /= null;
--     end Has_Government;

   ----------------
   -- Has_Market --
   ----------------

   function Has_Market
     (World : Root_World_Type'Class)
      return Boolean
   is
      use type Concorde.Markets.Market_Type;
   begin
      return World.Market /= null;
   end Has_Market;

   -----------------
   -- Hydrosphere --
   -----------------

   function Hydrosphere
     (World : Root_World_Type'Class)
      return Unit_Real
   is
   begin
      return World.Hydrosphere;
   end Hydrosphere;

   ------------------------
   -- Import_Market_Size --
   ------------------------

--     function Import_Market_Size
--       (World     : Root_World_Type'Class;
--        Commodity : Concorde.Commodities.Commodity_Type)
--        return Concorde.Quantities.Quantity_Type
--     is
--        use Concorde.Quantities;
--        Supply : constant Quantity_Type :=
--                   World.Market.Current_Supply (Commodity);
--        Demand : constant Quantity_Type :=
--                   World.Market.Current_Demand (Commodity);
--     begin
--        if Supply < Demand then
--           return Demand - Supply;
--        else
--           return Zero;
--        end if;
--     end Import_Market_Size;

   ----------------
   -- Is_Capital --
   ----------------

--     function Is_Capital
--       (World : Root_World_Type'Class)
--        return Boolean
--     is
--     begin
--        return World.Is_Capital_World;
--     end Is_Capital;

   -------------
   -- Is_Moon --
   -------------

   function Is_Moon
     (World : Root_World_Type'Class)
      return Boolean
   is
   begin
      return World.Moon;
   end Is_Moon;

   ----------
   -- Load --
   ----------

   overriding procedure Load (World : in out Root_World_Type) is
   begin
      if not World.Is_Gas_Giant
        and then not World.Is_Moon
      then
         Concorde.Worlds.Create.Create_Sector_Layout (World);
         Concorde.Worlds.Create.Create_Resources (World);
      end if;
   end Load;

   -------------------------
   -- Maximum_Temperature --
   -------------------------

   function Maximum_Temperature
     (World : Root_World_Type'Class)
      return Non_Negative_Real
   is
   begin
      return World.Max_Temperature;
   end Maximum_Temperature;

   -------------------
   -- Mine_Resource --
   -------------------

   overriding procedure Mine_Resource
     (World           : in out Root_World_Type;
      Resource        : Concorde.Commodities.Commodity_Type;
      Mine_Production : Non_Negative_Real;
      Mined           : out Non_Negative_Real)
   is
      Current : Deposit_Record :=
                  World.Resources.Element (Resource);
      Potential : constant Non_Negative_Real :=
                    Mine_Production * Current.Concentration
                      * Current.Accessibility;
   begin
      if Current.Abundance <= Potential then
         Mined := Current.Abundance * 0.9;
         Current.Abundance := Current.Abundance * 0.1;
         Current.Concentration := Current.Concentration * 0.1;
         Current.Accessibility := Current.Accessibility * 0.1;
      else
         Mined := Potential;
         declare
            Factor : constant Unit_Real :=
                       Unit_Real'Min
                         ((Current.Abundance - Mined) / Current.Abundance,
                          1.0 - 1.0E-6);
         begin
            Current.Concentration :=
              Current.Concentration * Factor;
            Current.Accessibility :=
              Current.Accessibility * Factor;
            Current.Abundance := Current.Abundance - Mined;
         end;
      end if;

      if Mined > 0.0 then
         World.Resources.Replace_Element (Resource, Current);
      end if;
   end Mine_Resource;

   -------------------------
   -- Minimum_Temperature --
   -------------------------

   function Minimum_Temperature
     (World : Root_World_Type'Class)
      return Non_Negative_Real
   is
   begin
      return World.Min_Temperature;
   end Minimum_Temperature;

   -----------
   -- Moons --
   -----------

   function Moons (World : Root_World_Type'Class) return Array_Of_Worlds is

      Result : Array_Of_Worlds (1 .. 20);
      Count  : Natural := 0;

      procedure Add_Moon
        (Item : not null access constant
           Concorde.Systems.Star_System_Object_Interface'Class);

      --------------
      -- Add_Moon --
      --------------

      procedure Add_Moon
        (Item : not null access constant
           Concorde.Systems.Star_System_Object_Interface'Class)
      is
         Primary : constant access constant
           Concorde.Systems.Star_System_Object_Interface'Class :=
             Item.Primary;
      begin
         if Primary /= null
           and then Primary.all in Root_World_Type'Class
           and then World_Type (Primary).Identifier = World.Identifier
         then
            Count := Count + 1;
            Result (Count) := World_Type (Item);
         end if;
      end Add_Moon;

   begin
      World.System.Scan_System_Objects
        (Add_Moon'Access);
      return Result (1 .. Count);
   end Moons;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (World : Root_World_Type)
      return Memor.Memor_Database
   is
      pragma Unreferenced (World);
   begin
      return Db.Get_Database;
   end Object_Database;

   -----------
   -- Owned --
   -----------

   function Owned
     (World : Root_World_Type'Class)
      return Boolean
   is
   begin
      return World.Owner /= null;
   end Owned;

   --------------
   -- Owned_By --
   --------------

   function Owned_By
     (World  : Root_World_Type'Class;
      Faction : Concorde.Factions.Root_Faction_Type'Class)
      return Boolean
   is
      use type Memor.Database_Reference;
   begin
      return World.Owner.Reference = Faction.Reference;
   end Owned_By;

   -----------
   -- Owner --
   -----------

   function Owner
     (World : Root_World_Type'Class)
      return access constant Concorde.Factions.Root_Faction_Type'Class
   is
   begin
      return World.Owner;
   end Owner;

   --------------------
   -- Scan_Resources --
   --------------------

   procedure Scan_Resources
     (World   : Root_World_Type'Class;
      Process : not null access
        procedure (Resource : Concorde.Commodities.Commodity_Type;
                   Concentration : Unit_Real;
                   Abundance     : Non_Negative_Real))
   is
      procedure Internal_Process
        (Commodity : not null access constant
           Concorde.Commodities.Root_Commodity_Type'Class;
         Deposit   : Deposit_Record);

      ----------------------
      -- Internal_Process --
      ----------------------

      procedure Internal_Process
        (Commodity : not null access constant
           Concorde.Commodities.Root_Commodity_Type'Class;
         Deposit   : Deposit_Record)
      is
      begin
         Process
           (Resource      => Concorde.Commodities.Commodity_Type (Commodity),
            Concentration => Deposit.Concentration,
            Abundance     => Deposit.Abundance);
      end Internal_Process;

   begin
      World.Resources.Scan
        (Skip_Default => True,
         Process      => Internal_Process'Access);
   end Scan_Resources;

   ----------------
   -- Scan_Ships --
   ----------------

   procedure Scan_Ships
     (World   : Root_World_Type'Class;
      Process : not null access
        procedure (Ship  : Concorde.Ships.Ship_Type))
   is
   begin
      for Ship of World.Ships loop
         Process (Ship);
      end loop;
   end Scan_Ships;

   -----------------
   -- Scan_Worlds --
   -----------------

   procedure Scan_Worlds
     (Process : not null access
        procedure (World : World_Type))
   is
   begin
      Db.Scan (Process);
   end Scan_Worlds;

   --------------------
   -- Sector_Feature --
   --------------------

   function Sector_Feature
     (World  : Root_World_Type'Class;
      Sector : Concorde.Surfaces.Surface_Tile_Index)
      return Concorde.Features.Feature_Type
   is
   begin
      return World.Sectors (Sector).Feature;
   end Sector_Feature;

   -------------------------
   -- Sector_Ground_Level --
   -------------------------

   function Sector_Ground_Level
     (World  : Root_World_Type'Class;
      Sector : Concorde.Surfaces.Surface_Tile_Index)
      return Real
   is
   begin
      return Real (World.Sectors (Sector).Height) * 100.0;
   end Sector_Ground_Level;

   ------------------------
   -- Sector_Has_Feature --
   ------------------------

   function Sector_Has_Feature
     (World  : Root_World_Type'Class;
      Sector : Concorde.Surfaces.Surface_Tile_Index)
      return Boolean
   is
      use type Concorde.Features.Feature_Type;
   begin
      return World.Sectors (Sector).Feature /= null;
   end Sector_Has_Feature;

   ------------------------
   -- Sector_Has_Terrain --
   ------------------------

   function Sector_Has_Terrain
     (World  : Root_World_Type'Class;
      Sector : Concorde.Surfaces.Surface_Tile_Index)
      return Boolean
   is
      use type Concorde.Terrain.Terrain_Type;
   begin
      return World.Sectors (Sector).Terrain /= null;
   end Sector_Has_Terrain;

   ---------------------
   -- Sector_Resource --
   ---------------------

   function Sector_Resource
     (World  : Root_World_Type'Class;
      Sector : Concorde.Surfaces.Surface_Tile_Index)
      return Concorde.Commodities.Commodity_Type
   is
   begin
      return World.Sectors (Sector).Deposit.Resource;
   end Sector_Resource;

   --------------------------------
   -- Sector_Temperature_Average --
   --------------------------------

   function Sector_Temperature_Average
     (World  : Root_World_Type'Class;
      Sector : Concorde.Surfaces.Surface_Tile_Index)
      return Non_Negative_Real
   is
   begin
      return World.Sectors (Sector).Temperature.Average;
   end Sector_Temperature_Average;

   -----------------------------
   -- Sector_Temperature_High --
   -----------------------------

   function Sector_Temperature_High
     (World  : Root_World_Type'Class;
      Sector : Concorde.Surfaces.Surface_Tile_Index)
      return Non_Negative_Real
   is
   begin
      return World.Sectors (Sector).Temperature.High;
   end Sector_Temperature_High;

   ----------------------------
   -- Sector_Temperature_Low --
   ----------------------------

   function Sector_Temperature_Low
     (World  : Root_World_Type'Class;
      Sector : Concorde.Surfaces.Surface_Tile_Index)
      return Non_Negative_Real
   is
   begin
      return World.Sectors (Sector).Temperature.Low;
   end Sector_Temperature_Low;

   --------------------
   -- Sector_Terrain --
   --------------------

   function Sector_Terrain
     (World  : Root_World_Type'Class;
      Sector : Concorde.Surfaces.Surface_Tile_Index)
      return Concorde.Terrain.Terrain_Type
   is
   begin
      return World.Sectors (Sector).Terrain;
   end Sector_Terrain;

   ----------
   -- Sell --
   ----------

--     procedure Sell
--       (World     : in out Root_World_Type'Class;
--        Commodity : Concorde.Commodities.Commodity_Type;
--        Quantity  : in out Concorde.Quantities.Quantity_Type)
--     is
--        use Concorde.Money, Concorde.Quantities;
--        New_Quantity : constant Concorde.Quantities.Quantity_Type :=
--                     Min (Quantity, World.Export_Market_Size (Commodity));
--        Cash         : constant Money_Type :=
--                         Total (World.Sell_Price (Commodity), New_Quantity);
--     begin
--
--        declare
--           Upd : constant Concorde.Installations.Updateable_Reference :=
--                   World.Port.Update;
--        begin
--           Upd.Add_Cash (Cash);
--           Upd.Remove_Quantity (Commodity, New_Quantity, Cash);
--
--        end;
--
--        Quantity := New_Quantity;
--     end Sell;

   ----------------
   -- Sell_Price --
   ----------------

--     function Sell_Price
--       (World     : Root_World_Type'Class;
--        Commodity : Concorde.Commodities.Commodity_Type)
--        return Concorde.Money.Price_Type
--     is
--  --        use Concorde.Money;
--        use Concorde.Quantities;
--        Base_Price : constant Concorde.Money.Price_Type :=
--                       (if World.Port.Get_Quantity (Commodity) > Zero
--                        then World.Port.Get_Average_Price (Commodity)
--                        else World.Market.Current_Price (Commodity));
--     begin
--        World.Port.Log_Price
--          (Commodity.Name & ": stock price "
--           & (if World.Port.Get_Quantity (Commodity) > Zero
--             then Image (World.Port.Get_Average_Price (Commodity))
--             else "-")
--           & "; local average "
--           & Image (World.Market.Last_Average_Bid (Commodity))
--           & "; with export tax "
--           & Image (Concorde.Money.Add_Tax
--             (Base_Price,
--                  World.Government.Tax_Rate
--                    (Concorde.Trades.Export, Commodity))));

--        return Concorde.Money.Adjust_Price
--          (Concorde.Money.Add_Tax
--             (Base_Price,
--              Float
--                (World.Government.Tax_Rate
--                     (Concorde.Trades.Export, Commodity))),
--           1.1);
--     end Sell_Price;

   --------------------
   -- Set_Government --
   --------------------

--     procedure Set_Government
--       (World      : in out Root_World_Type'Class;
--        Government : Concorde.Government.Government_Type)
--     is
--
--        procedure Set_Initial_Prices
--          (Market : Concorde.Markets.Updateable_Reference);
--
--        ------------------------
--        -- Set_Initial_Prices --
--        ------------------------
--
--        procedure Set_Initial_Prices
--          (Market : Concorde.Markets.Updateable_Reference)
--        is
--           Factor : Non_Negative_Real := 0.1;
--        begin
--           for Resource of World.Resources loop
--              Market.Initial_Price
--                (Resource,
--                 Concorde.Money.Adjust_Price
--                   (Resource.Base_Price, Float (Factor)));
--              Factor := Factor + (0.8 - Factor) / 2.0;
--           end loop;
--        end Set_Initial_Prices;
--
--     begin
--        World.Government := Government;
--        World.Market :=
--          Concorde.Markets.Create_Market
--            (World.Identifier,
--             Concorde.Worlds.Db.Reference (World.Reference),
--             World.Government,
--             Enable_Logging => Concorde.Options.Enable_Market_Logging);
--
--        Government.Update.Set_Market (World.Market);
--
--        Set_Initial_Prices (World.Market.Update);
--
--     end Set_Government;

   ----------------
   -- Set_Height --
   ----------------

   overriding procedure Set_Height
     (World       : in out Root_World_Type;
      Tile_Index  : Positive;
      Height      : Positive)
   is
      Sector : Sector_Record renames
                 World.Sectors
                   (Concorde.Surfaces.Surface_Tile_Index (Tile_Index));

   begin
      Sector.Height :=
        Height_Range (Height + Min_Height - 1);
   end Set_Height;

   ------------------
   -- Set_Location --
   ------------------

   overriding procedure Set_Location
     (World    : in out Root_World_Type;
      Location : Concorde.Locations.Object_Location)
   is
   begin
      World.Location := Location;
   end Set_Location;

   ----------------
   -- Set_Market --
   ----------------

   procedure Set_Market
     (World  : in out Root_World_Type'Class;
      Market : Concorde.Markets.Market_Type)
   is
   begin
      World.Market := Market;
   end Set_Market;

   ---------------
   -- Set_Owner --
   ---------------

   procedure Set_Owner
     (World  : in out Root_World_Type'Class;
      Faction : not null access constant
        Concorde.Factions.Root_Faction_Type'Class)
   is
   begin
      World.Owner := Concorde.Factions.Faction_Type (Faction);
   end Set_Owner;

   ----------------------
   -- Total_Population --
   ----------------------

   function Total_Population
     (World : Root_World_Type'Class)
      return Concorde.Quantities.Quantity_Type
   is
      use Concorde.Quantities;
   begin
      return Result : Quantity_Type := Zero do
         for Community of World.Communities loop
            Result := Result + Community.Total_Population;
         end loop;
      end return;
   end Total_Population;

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant Root_World_Type'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update;

end Concorde.Worlds;
