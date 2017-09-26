with Concorde.Solar_System;

package body Concorde.Stars is

   ---------
   -- Age --
   ---------

   overriding function Age
     (Star : Root_Star_Type)
      return Non_Negative_Real
   is
   begin
      return Star.Age;
   end Age;

   ------------
   -- Colour --
   ------------

   overriding function Colour
     (Star : Root_Star_Type)
      return Lui.Colours.Colour_Type
   is
   begin
      return Star.Colour;
   end Colour;

   ---------------
   -- Ecosphere --
   ---------------

   function Ecosphere
     (Star : Root_Star_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Star.Ecosphere;
   end Ecosphere;

   ----------------
   -- Luminosity --
   ----------------

   function Luminosity
     (Star : Root_Star_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Star.Luminosity;
   end Luminosity;

   ----------
   -- Mass --
   ----------

   overriding function Mass
     (Star : Root_Star_Type)
      return Non_Negative_Real
   is
   begin
      return Star.Solar_Masses * Concorde.Solar_System.Solar_Mass;
   end Mass;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Star : Root_Star_Type)
      return Memor.Memor_Database
   is
      pragma Unreferenced (Star);
   begin
      return Db.Get_Database;
   end Object_Database;

   ------------
   -- Radius --
   ------------

   overriding function Radius
     (Star : Root_Star_Type)
      return Non_Negative_Real
   is
   begin
      return Star.Radius;
   end Radius;

   ------------------
   -- Solar_Masses --
   ------------------

   function Solar_Masses
     (Star : Root_Star_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Star.Solar_Masses;
   end Solar_Masses;

   -------------------
   -- Stellar_Class --
   -------------------

   function Stellar_Class
     (Star : Root_Star_Type'Class)
      return String
   is
   begin
      return Stellar_Class_Type'Image (Star.Class)
        & Character'Val (Star.Subclass + Character'Pos ('0'));
   end Stellar_Class;

   -------------------
   -- Stellar_Class --
   -------------------

   function Stellar_Class
     (Star : Root_Star_Type'Class)
      return Stellar_Class_Type
   is
   begin
      return Star.Class;
   end Stellar_Class;

end Concorde.Stars;
