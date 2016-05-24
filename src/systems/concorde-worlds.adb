with Concorde.Empires;
with Concorde.Government.Db;
with Concorde.Markets.Db;
with Concorde.Worlds.Db;
with Concorde.Money;

package body Concorde.Worlds is

   ----------------------
   -- Add_Installation --
   ----------------------

   procedure Add_Installation
     (World        : in out Root_World_Type'Class;
      Sector       : Concorde.Surfaces.Surface_Tile_Index;
      Installation : Concorde.Installations.Installation_Type)
   is
   begin
      World.Sectors (Sector).Installations.Append (Installation);
   end Add_Installation;

   -------------
   -- Add_Pop --
   -------------

   procedure Add_Pop
     (World  : in out Root_World_Type'Class;
      Sector : Concorde.Surfaces.Surface_Tile_Index;
      Pop    : Concorde.People.Pops.Pop_Type)
   is
   begin
      World.Sectors (Sector).Pops.Append (Pop);
   end Add_Pop;

   --------------
   -- Add_Ship --
   --------------

   procedure Add_Ship
     (World : in out Root_World_Type'Class;
      Ship  : Concorde.Ships.Ship_Type)
   is
   begin
      World.Ships.Append (Ship);
   end Add_Ship;

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
      Tile : constant Concorde.Surfaces.Surface_Tile_Index :=
               Concorde.Surfaces.Surface_Tile_Index
                 (Concorde.Locations.World_Sector (Location));
      Deposit : Deposit_Record renames
                  World.Sectors (Tile).Deposit;
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

   ----------------
   -- Government --
   ----------------

   function Government
     (World : Root_World_Type'Class)
      return Concorde.Government.Government_Type
   is
   begin
      return World.Government;
   end Government;

   --------------------
   -- Has_Government --
   --------------------

   function Has_Government
     (World : Root_World_Type'Class)
      return Boolean
   is
      use type Concorde.Government.Government_Type;
   begin
      return World.Government /= null;
   end Has_Government;

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

   ----------------
   -- Is_Capital --
   ----------------

   function Is_Capital
     (World : Root_World_Type'Class)
      return Boolean
   is
   begin
      return World.Is_Capital_World;
   end Is_Capital;

   ------------
   -- Market --
   ------------

   function Market
     (World : Root_World_Type'Class)
      return Concorde.Markets.Market_Type
   is
   begin
      return World.Market;
   end Market;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (World : Root_World_Type)
      return Memor.Root_Database_Type'Class
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
      use type Concorde.Empires.Empire_Type;
   begin
      return World.Owner /= null;
   end Owned;

   --------------
   -- Owned_By --
   --------------

   function Owned_By
     (World  : Root_World_Type'Class;
      Empire : Concorde.Empires.Root_Empire_Type'Class)
      return Boolean
   is
      use type Memor.Database_Reference;
   begin
      return World.Owner.Reference = Empire.Reference;
   end Owned_By;

   -----------
   -- Owner --
   -----------

   function Owner
     (World : Root_World_Type'Class)
      return access constant Concorde.Empires.Root_Empire_Type'Class
   is
   begin
      return World.Owner;
   end Owner;

   ---------------
   -- Resources --
   ---------------

   function Resources
     (World : Root_World_Type'Class)
      return Concorde.Commodities.Array_Of_Commodities
   is
      use Concorde.Commodities;
      Count : constant Natural := Natural (World.Resources.Length);
      Index : Positive := 1;
   begin
      return Result : Array_Of_Commodities (1 .. Count) do
         for Resource of World.Resources loop
            Result (Index) := Resource;
            Index := Index + 1;
         end loop;
      end return;
   end Resources;

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

   -----------------
   -- Set_Capital --
   -----------------

   procedure Set_Capital
     (World      : in out Root_World_Type'Class;
      Is_Capital : Boolean)
   is
   begin
      World.Is_Capital_World := Is_Capital;
   end Set_Capital;

   --------------------
   -- Set_Government --
   --------------------

   procedure Set_Government
     (World      : in out Root_World_Type'Class;
      Government : Concorde.Government.Government_Type)
   is

      procedure Set_Initial_Prices
        (Market : in out Concorde.Markets.Root_Market_Type'Class);

      procedure Set_Market
        (Government : in out Concorde.Government.Root_Government_Type'Class);

      ------------------------
      -- Set_Initial_Prices --
      ------------------------

      procedure Set_Initial_Prices
        (Market : in out Concorde.Markets.Root_Market_Type'Class)
      is
         Factor : Non_Negative_Real := 0.1;
      begin
         for Resource of World.Resources loop
            Market.Initial_Price
              (Resource,
               Concorde.Money.Adjust_Price
                 (Resource.Base_Price, Factor));
            Factor := Factor + (0.8 - Factor) / 2.0;
         end loop;
      end Set_Initial_Prices;

      ----------------
      -- Set_Market --
      ----------------

      procedure Set_Market
        (Government : in out Concorde.Government.Root_Government_Type'Class)
      is
      begin
         Government.Set_Market (World.Market);
      end Set_Market;

   begin
      World.Government := Government;
      World.Market :=
        Concorde.Markets.Create_Market
          (Concorde.Worlds.Db.Reference (World.Reference),
           World.Government,
           Enable_Logging => False);
      Concorde.Government.Db.Update
        (Government.Reference, Set_Market'Access);
      Concorde.Markets.Db.Update
        (World.Market.Reference, Set_Initial_Prices'Access);
   end Set_Government;

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

   ---------------
   -- Set_Owner --
   ---------------

   procedure Set_Owner
     (World  : in out Root_World_Type'Class;
      Empire : not null access constant
        Concorde.Empires.Root_Empire_Type'Class)
   is
   begin
      World.Owner := Concorde.Empires.Empire_Type (Empire);
   end Set_Owner;

   ------------
   -- System --
   ------------

   function System
     (World : Root_World_Type'Class)
      return Concorde.Systems.Star_System_Type
   is
   begin
      return World.System;
   end System;

end Concorde.Worlds;
