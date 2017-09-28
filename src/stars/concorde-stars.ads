private with Memor;
private with Memor.Database;

with Lui.Colours;

with Concorde.Objects;

with Concorde.Systems;

with Concorde.Commodities;

private with Concorde.Geometry;
with Concorde.Locations;
with Concorde.Dates;

package Concorde.Stars is

   type Stellar_Class_Type is (O, B, A, F, G, K, M, L);
   type Stellar_Subclass_Type is range 0 .. 9;
   type Stellar_Size_Type is (Brown_Dwarf, Main_Sequence, Giant, Supergiant);

   type Stellar_Classification is
      record
         Class    : Stellar_Class_Type;
         Subclass : Stellar_Subclass_Type;
         Size     : Stellar_Size_Type;
      end record;

   type Root_Star_Type is
     new Concorde.Objects.Root_User_Named_Object_Type
     and Concorde.Systems.Main_Star_System_Object_Interface
   with private;

   function Stellar_Class
     (Star : Root_Star_Type'Class)
      return String;

   function Stellar_Class
     (Star : Root_Star_Type'Class)
      return Stellar_Class_Type;

   function Solar_Masses
     (Star : Root_Star_Type'Class)
      return Non_Negative_Real;

   function Luminosity
     (Star : Root_Star_Type'Class)
      return Non_Negative_Real;

   function Ecosphere
     (Star : Root_Star_Type'Class)
      return Non_Negative_Real;

   overriding function Age
     (Star : Root_Star_Type)
      return Non_Negative_Real;
   --  Age of star in Earth years

   overriding function Colour
     (Star : Root_Star_Type)
      return Lui.Colours.Colour_Type;

   type Star_Type is access constant Root_Star_Type'Class;

private

   type Root_Star_Type is
     new Concorde.Objects.Root_User_Named_Object_Type
     and Concorde.Systems.Main_Star_System_Object_Interface with
      record
         System       : Concorde.Systems.Star_System_Type;
         Location     : Concorde.Locations.Object_Location;
         Class        : Stellar_Class_Type;
         Subclass     : Stellar_Subclass_Type;
         Size         : Stellar_Size_Type;
         Solar_Masses : Non_Negative_Real;
         Age          : Non_Negative_Real;
         Colour       : Lui.Colours.Colour_Type;
         Radius       : Non_Negative_Real;
         Luminosity   : Non_Negative_Real;
         Ecosphere    : Non_Negative_Real;
      end record;

   overriding function Object_Database
     (Star : Root_Star_Type)
      return Memor.Memor_Database;

   overriding function Mass
     (Star : Root_Star_Type)
      return Non_Negative_Real;

   overriding function Radius
     (Star : Root_Star_Type)
      return Non_Negative_Real;

   overriding function Primary
     (Star : Root_Star_Type)
      return access Concorde.Systems.Star_System_Object_Interface'Class
   is (null);

   overriding function Current_Location
     (Star : Root_Star_Type)
      return Concorde.Locations.Object_Location
   is (Star.Location);

   overriding function Location_At
     (Star : Root_Star_Type;
      Time    : Concorde.Dates.Date_Type)
      return Concorde.Locations.Object_Location
   is (Concorde.Locations.Location_At (Star.Location, Time));

   overriding procedure Set_Location
     (Star    : in out Root_Star_Type;
      Location : Concorde.Locations.Object_Location);

   overriding function Semimajor_Axis
     (Star : Root_Star_Type)
      return Non_Negative_Real
   is (0.0);

   overriding function Eccentricity
     (Star : Root_Star_Type)
      return Unit_Real
   is (0.0);

   overriding function Orbit_Progress
     (Star : Root_Star_Type)
      return Concorde.Geometry.Radians
   is (Concorde.Geometry.Degrees_To_Radians (0.0));

   overriding function System
     (Star : Root_Star_Type)
      return access constant Concorde.Systems.Root_Star_System_Type'Class
   is (Star.System);

   package Db is
     new Memor.Database
       ("star", Root_Star_Type, Star_Type);

end Concorde.Stars;
