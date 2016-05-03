private with Memor;

with Concorde.Objects;

package Concorde.Atmosphere is

   type Root_Gas_Type is
     new Concorde.Objects.Root_Named_Object_Type
   with private;

   function Abundance_E
     (Gas : Root_Gas_Type'Class)
      return Non_Negative_Real;

   function Abundance_S
     (Gas : Root_Gas_Type'Class)
      return Non_Negative_Real;

   function Boiling_Point
     (Gas : Root_Gas_Type'Class)
      return Non_Negative_Real;

   function Density
     (Gas : Root_Gas_Type'Class)
      return Non_Negative_Real;

   function Formula
     (Gas : Root_Gas_Type'Class)
      return String;

   function Max_IPP
     (Gas : Root_Gas_Type'Class)
      return Non_Negative_Real;

   function Melting_Point
     (Gas : Root_Gas_Type'Class)
      return Non_Negative_Real;

   function Molecular_Weight
     (Gas : Root_Gas_Type'Class)
      return Non_Negative_Real;

   function Reactivity
     (Gas : Root_Gas_Type'Class)
      return Non_Negative_Real;

   type Gas_Type is access constant Root_Gas_Type'Class;

   type Array_Of_Gases is array (Positive range <>) of Gas_Type;

   function By_Molecular_Weight return Array_Of_Gases;

private

   type Root_Gas_Type is
     new Concorde.Objects.Root_Named_Object_Type with
      record
         Formula          : access String;
         Melting_Point    : Non_Negative_Real;
         Boiling_Point    : Non_Negative_Real;
         Molecular_Weight : Non_Negative_Real;
         Density          : Non_Negative_Real;
         Abundance_E      : Non_Negative_Real;
         Abundance_S      : Non_Negative_Real;
         Reactivity       : Non_Negative_Real;
         Max_IPP          : Non_Negative_Real;
      end record;

   overriding function Object_Database
     (Gas : Root_Gas_Type)
      return Memor.Root_Database_Type'Class;

end Concorde.Atmosphere;
