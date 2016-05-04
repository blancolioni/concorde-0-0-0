private with Ada.Containers.Doubly_Linked_Lists;

private with Memor;

with Concorde.Objects;

with Concorde.Atmosphere;
with Concorde.Features;
with Concorde.Terrain;

with Concorde.Systems;

with Concorde.Commodities;

package Concorde.Worlds is

   type World_Category is
     (Rock, Martian, Venusian, Terrestrial, Water, Ice, Sub_Jovian, Jovian);

   type Root_World_Type is
     new Concorde.Objects.Root_Named_Object_Type
     and Concorde.Systems.Star_System_Object_Interface
   with private;

   function Category
     (World : Root_World_Type'Class)
      return World_Category;

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

   type Sector_Record is
      record
         Terrain : Concorde.Terrain.Terrain_Type;
         Feature : Concorde.Features.Feature_Type;
         Deposit : Deposit_Record;
      end record;

   type Array_Of_Sectors is
     array (Positive range <>) of Sector_Record;

   type Array_Of_Row_Lengths is array (Positive range <>) of Positive;

   World_Sector_Size : constant := 1.0E6;

   type Root_World_Type is
     new Concorde.Objects.Root_Named_Object_Type
     and Concorde.Systems.Star_System_Object_Interface with
      record
         Primary               : access
           Concorde.Systems.Star_System_Object_Interface'Class;
         Semimajor_Axis        : Non_Negative_Real;
         Eccentricity          : Unit_Real;
         Category              : World_Category;
         Surface_Seed          : Integer;
         Sectors               : access Array_Of_Sectors;
         Row_Length            : access Array_Of_Row_Lengths;
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
         Great_Circle_Sectors  : Natural;
         Half_Circle_Sectors   : Natural;
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

   overriding function Semimajor_Axis
     (World : Root_World_Type)
      return Non_Negative_Real
   is (World.Semimajor_Axis);

   overriding function Eccentricity
     (World : Root_World_Type)
      return Unit_Real
   is (World.Eccentricity);

end Concorde.Worlds;
