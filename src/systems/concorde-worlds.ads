private with Ada.Containers.Doubly_Linked_Lists;

private with Memor;

private with Concorde.Geometry;

with Concorde.Money;
with Concorde.Quantities;

with Concorde.Maps;
with Concorde.Surfaces;

with Concorde.Objects;

with Concorde.Atmosphere;
with Concorde.Features;
with Concorde.Terrain;

with Concorde.Systems;

with Concorde.Commodities;
with Concorde.Government;
with Concorde.People.Pops;
with Concorde.Installations;
with Concorde.Markets;
with Concorde.Trades;

limited with Concorde.Empires;
with Concorde.Ships.Lists;

with Concorde.Locations;

private with Concorde.Commodities.Lists;
private with Concorde.Installations.Lists;
private with Concorde.People.Pops.Lists;

package Concorde.Worlds is

   type World_Category is
     (Rock, Martian, Venusian, Terrestrial, Water, Ice, Sub_Jovian, Jovian);

   subtype Rocky_World is World_Category range Rock .. Ice;
   subtype Jovian_World is World_Category range Sub_Jovian .. Jovian;

   type Root_World_Type is
     new Concorde.Objects.Root_User_Named_Object_Type
     and Concorde.Systems.Star_System_Object_Interface
     and Concorde.Government.Governed_Interface
     and Concorde.Maps.Tile_Layout_Interface
   with private;

   function Day_Length
     (World : Root_World_Type'Class)
      return Non_Negative_Real;

   function Owner
     (World : Root_World_Type'Class)
      return access constant Concorde.Empires.Root_Empire_Type'Class;

   function Owned
     (World : Root_World_Type'Class)
      return Boolean;

   function Owned_By
     (World : Root_World_Type'Class;
      Empire : Concorde.Empires.Root_Empire_Type'Class)
      return Boolean;

   procedure Set_Owner
     (World  : in out Root_World_Type'Class;
      Empire : not null access constant
        Concorde.Empires.Root_Empire_Type'Class);

   function Category
     (World : Root_World_Type'Class)
      return World_Category;

   function Is_Moon
     (World : Root_World_Type'Class)
      return Boolean;

   function Maximum_Temperature
     (World : Root_World_Type'Class)
      return Non_Negative_Real;

   function Minimum_Temperature
     (World : Root_World_Type'Class)
      return Non_Negative_Real;

   function Hydrosphere
     (World : Root_World_Type'Class)
      return Unit_Real;

   function Gas_Giant
     (World : Root_World_Type'Class)
      return Boolean
   is (World.Category in Jovian_World);

   procedure Set_Capital
     (World      : in out Root_World_Type'Class;
      Is_Capital : Boolean);

   function Is_Capital
     (World : Root_World_Type'Class)
      return Boolean;

   procedure Set_Government
     (World      : in out Root_World_Type'Class;
      Government : Concorde.Government.Government_Type);

   function Has_Government
     (World : Root_World_Type'Class)
      return Boolean;

   function Government
     (World : Root_World_Type'Class)
      return Concorde.Government.Government_Type;

   function Has_Market
     (World : Root_World_Type'Class)
      return Boolean;

   function Market
     (World : Root_World_Type'Class)
      return Concorde.Markets.Market_Type;

   function Resources
     (World : Root_World_Type'Class)
      return Concorde.Commodities.Array_Of_Commodities;

   function Sector_Ground_Level
     (World  : Root_World_Type'Class;
      Sector : Concorde.Surfaces.Surface_Tile_Index)
      return Real;

   function Sector_Has_Feature
     (World  : Root_World_Type'Class;
      Sector : Concorde.Surfaces.Surface_Tile_Index)
      return Boolean;

   function Sector_Has_Terrain
     (World  : Root_World_Type'Class;
      Sector : Concorde.Surfaces.Surface_Tile_Index)
      return Boolean;

   function Sector_Resource
     (World  : Root_World_Type'Class;
      Sector : Concorde.Surfaces.Surface_Tile_Index)
      return Concorde.Commodities.Commodity_Type;

   function Sector_Temperature_Low
     (World  : Root_World_Type'Class;
      Sector : Concorde.Surfaces.Surface_Tile_Index)
      return Non_Negative_Real;

   function Sector_Temperature_Average
     (World  : Root_World_Type'Class;
      Sector : Concorde.Surfaces.Surface_Tile_Index)
      return Non_Negative_Real;

   function Sector_Temperature_High
     (World  : Root_World_Type'Class;
      Sector : Concorde.Surfaces.Surface_Tile_Index)
      return Non_Negative_Real;

   function Sector_Feature
     (World  : Root_World_Type'Class;
      Sector : Concorde.Surfaces.Surface_Tile_Index)
      return Concorde.Features.Feature_Type
     with Pre => World.Sector_Has_Feature (Sector);

   function Sector_Terrain
     (World  : Root_World_Type'Class;
      Sector : Concorde.Surfaces.Surface_Tile_Index)
      return Concorde.Terrain.Terrain_Type
     with Pre => World.Sector_Has_Terrain (Sector);

   procedure Get_Sector_Resource
     (Location      : Concorde.Locations.Object_Location;
      Resource      : out Concorde.Commodities.Commodity_Type;
      Concentration : out Unit_Real;
      Accessibility : out Unit_Real);

   procedure Get_Sector_Resource
     (World         : Root_World_Type'Class;
      Sector        : Concorde.Surfaces.Surface_Tile_Index;
      Resource      : out Concorde.Commodities.Commodity_Type;
      Concentration : out Unit_Real;
      Accessibility : out Unit_Real);

   --     function Resource
--       (System : Root_Star_System_Type'Class)
--        return Concorde.Commodities.Commodity_Type;
--
--     function Resource_Accessibility
--       (System : Root_Star_System_Type'Class)
--        return Unit_Real;
--
--     function Resource_Concentration
--       (System : Root_Star_System_Type'Class)
--        return Unit_Real;
--
--     function Resource_Size
--       (System : Root_Star_System_Type'Class)
--        return Concorde.Quantities.Quantity;

   procedure Add_Pop
     (World  : in out Root_World_Type'Class;
      Sector : Concorde.Surfaces.Surface_Tile_Index;
      Pop    : Concorde.People.Pops.Pop_Type);

   procedure Add_Installation
     (World        : in out Root_World_Type'Class;
      Sector       : Concorde.Surfaces.Surface_Tile_Index;
      Installation : Concorde.Installations.Installation_Type);

   procedure Add_Ship
     (World : in out Root_World_Type'Class;
      Ship  : Concorde.Ships.Ship_Type);

   procedure Get_Ships
     (World : Root_World_Type'Class;
      Ships : out Concorde.Ships.Lists.List);

   function Buy_Price
     (World     : Root_World_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type;

   function Sell_Price
     (World     : Root_World_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Price_Type;

   function Import_Market_Size
     (World     : Root_World_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity;

   function Export_Market_Size
     (World     : Root_World_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity;

   procedure Buy
     (World     : in out Root_World_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : in out Concorde.Quantities.Quantity);

   procedure Sell
     (World     : in out Root_World_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : in out Concorde.Quantities.Quantity);

   type World_Type is access constant Root_World_Type'Class;

private

   type Atmospheric_Element is
      record
         Gas      : Concorde.Atmosphere.Gas_Type;
         Fraction : Unit_Real;
      end record;

   package Atmosphere_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Atmospheric_Element);

   type Deposit_Record is
      record
         Resource      : Concorde.Commodities.Commodity_Type;
         Accessibility : Unit_Real;
         Concentration : Unit_Real;
      end record;

   type Temperature_Record is
      record
         Low, Average, High : Non_Negative_Real;
      end record;

   type Wind_Record is
      record
         Direction : Concorde.Geometry.Radians;
         Speed     : Non_Negative_Real;
      end record;

   Min_Height : constant := -16;
   Max_Height : constant := 32;

   type Height_Range is range Min_Height .. Max_Height;

   type Sector_Record is
      record
         Height        : Height_Range;
         Terrain       : Concorde.Terrain.Terrain_Type;
         Feature       : Concorde.Features.Feature_Type;
         Deposit       : Deposit_Record;
         Temperature   : Temperature_Record;
         Wind          : Wind_Record;
         Moisture      : Non_Negative_Real;
         Pops          : Concorde.People.Pops.Lists.List;
         Installations : Concorde.Installations.Lists.List;
      end record;

   type Array_Of_Sectors is
     array (Concorde.Surfaces.Surface_Tile_Index range <>) of Sector_Record;

   type Height_Map is
     array (Positive range <>) of Height_Range
     with Component_Size => 8;

   type Root_World_Type is
     new Concorde.Objects.Root_User_Named_Object_Type
     and Concorde.Systems.Star_System_Object_Interface
     and Concorde.Government.Governed_Interface
     and Concorde.Maps.Tile_Layout_Interface with
      record
         System                : Concorde.Systems.Star_System_Type;
         Owner                 : access constant
           Concorde.Empires.Root_Empire_Type'Class;
         Primary               : access constant
           Concorde.Systems.Star_System_Object_Interface'Class;
         Semimajor_Axis        : Non_Negative_Real;
         Eccentricity          : Unit_Real;
         Orbit_Progress        : Concorde.Geometry.Radians;
         Category              : World_Category;
         Moon                  : Boolean := False;
         Surface_Seed          : Integer;
         Sectors               : access Array_Of_Sectors;
         Surface               : Concorde.Surfaces.Surface_Type;
         Resonant_Period       : Boolean;
         Greenhouse_Effect     : Boolean;
         Atmosphere            : Atmosphere_Lists.List;
         Mass                  : Non_Negative_Real;
         Radius                : Non_Negative_Real;
         Density               : Non_Negative_Real;
         Tilt                  : Non_Negative_Real;
         Solid_Mass            : Non_Negative_Real;
         Gas_Mass              : Non_Negative_Real;
         Core_Radius           : Non_Negative_Real;
         Day_Length            : Non_Negative_Real;
         Escape_Velocity       : Non_Negative_Real;
         Surface_Acceleration  : Non_Negative_Real;
         Surface_Gravity       : Non_Negative_Real;
         RMS_Velocity          : Non_Negative_Real;
         Min_Molecular_Weight  : Non_Negative_Real;
         Volatile_Gas_Inv      : Non_Negative_Real;
         Surface_Pressure      : Non_Negative_Real;
         Water_Boiling_Point   : Non_Negative_Real;
         Albedo                : Non_Negative_Real;
         Exospheric_Temp       : Non_Negative_Real;
         Surface_Temp          : Non_Negative_Real;
         Greenhouse_Rise       : Non_Negative_Real;
         Daytime_High          : Non_Negative_Real;
         Nighttime_Low         : Non_Negative_Real;
         Max_Temperature       : Non_Negative_Real;
         Min_Temperature       : Non_Negative_Real;
         Hydrosphere           : Unit_Real;
         Cloud_Cover           : Unit_Real;
         Ice_Cover             : Unit_Real;
         Sector_Count          : Natural;
         Is_Capital_World      : Boolean := False;
         Hub                   : Concorde.Installations.Installation_Type;
         Port                  : Concorde.Installations.Installation_Type;
         Resources             : Concorde.Commodities.Lists.List;
         Ships                 : Concorde.Ships.Lists.List;
         Market                : Concorde.Markets.Market_Type;
         Government            : Concorde.Government.Government_Type;
      end record;

   overriding function Object_Database
     (World : Root_World_Type)
      return Memor.Root_Database_Type'Class;

   overriding function Mass
     (World : Root_World_Type)
      return Non_Negative_Real
   is (World.Mass);

   overriding function Radius
     (World : Root_World_Type)
      return Non_Negative_Real
   is (World.Radius);

   overriding function Primary
     (World : Root_World_Type)
      return access Concorde.Systems.Star_System_Object_Interface'Class
   is (World.Primary);

   overriding function Age
     (World : Root_World_Type)
      return Non_Negative_Real
   is (World.Primary.Age);

   overriding function Semimajor_Axis
     (World : Root_World_Type)
      return Non_Negative_Real
   is (World.Semimajor_Axis);

   overriding function Eccentricity
     (World : Root_World_Type)
      return Unit_Real
   is (World.Eccentricity);

   overriding function Orbit_Progress
     (World : Root_World_Type)
      return Concorde.Geometry.Radians
   is (World.Orbit_Progress);

   overriding function Tile_Count
     (World : Root_World_Type)
      return Natural
   is (Natural (World.Sectors'Length));

   overriding function Neighbour_Count
     (World      : Root_World_Type;
      Tile_Index : Positive)
      return Natural
   is (Natural
       (World.Surface.Neighbour_Count
          (Concorde.Surfaces.Surface_Tile_Index (Tile_Index))));

   overriding function Neighbour
     (World           : Root_World_Type;
      Tile_Index      : Positive;
      Neighbour_Index : Positive)
      return Positive
   is (Positive
       (World.Surface.Neighbour
        (Concorde.Surfaces.Surface_Tile_Index (Tile_Index),
         Concorde.Surfaces.Tile_Neighbour_Index (Neighbour_Index))));

   overriding procedure Set_Height
     (World      : in out Root_World_Type;
      Tile_Index : Positive;
      Height     : Positive);

   overriding function System
     (World : Root_World_Type)
      return access constant Concorde.Systems.Root_Star_System_Type'Class
   is (World.System);

   overriding procedure Load (World : in out Root_World_Type);

end Concorde.Worlds;
