private with Memor;

with Lui.Colours;

with Concorde.Objects;

with Concorde.Systems;

with Concorde.Commodities;

package Concorde.Stars is

   type Stellar_Class_Type is (O, B, A, F, G, K, M, L);
   type Stellar_Subclass_Type is range 0 .. 9;
   type Stellar_Size_Type is (Brown_Dwarf, Main_Sequence, Giant, Supergiant);

   type Root_Star_Type is
     new Concorde.Objects.Root_Named_Object_Type
     and Concorde.Systems.Star_System_Object_Interface
   with private;

   function Stellar_Class
     (Star : Root_Star_Type'Class)
      return String;

   function Solar_Masses
     (Star : Root_Star_Type'Class)
      return Non_Negative_Real;

   overriding function Colour
     (Star : Root_Star_Type)
      return Lui.Colours.Colour_Type;

   type Star_Type is access constant Root_Star_Type'Class;

private

   type Root_Star_Type is
     new Concorde.Objects.Root_Named_Object_Type
     and Concorde.Systems.Star_System_Object_Interface with
      record
         Class        : Stellar_Class_Type;
         Subclass     : Stellar_Subclass_Type;
         Size         : Stellar_Size_Type;
         Solar_Masses : Non_Negative_Real;
         Colour       : Lui.Colours.Colour_Type;
         Radius       : Non_Negative_Real;
         Luminosity   : Non_Negative_Real;
      end record;

   overriding function Object_Database
     (Star : Root_Star_Type)
      return Memor.Root_Database_Type'Class;

   overriding function Mass
     (Star : Root_Star_Type)
      return Non_Negative_Real;

   overriding function Radius
     (Star : Root_Star_Type)
      return Non_Negative_Real;

end Concorde.Stars;
