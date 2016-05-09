private with Memor;

with Lui.Colours;

with Concorde.Objects;

package Concorde.Features is

   type Root_Feature_Type is
     new Concorde.Objects.Root_Localised_Object_Type
   with private;

   function Colour
     (Feature : Root_Feature_Type'Class)
      return Lui.Colours.Colour_Type;

   type Feature_Type is access constant Root_Feature_Type'Class;

   function Get (Id : String) return Feature_Type;

   function Ice return Feature_Type;
   function Desert return Feature_Type;

private

   type Root_Feature_Type is
     new Concorde.Objects.Root_Localised_Object_Type with
      record
         Colour    : Lui.Colours.Colour_Type;
         Is_Ice    : Boolean;
         Is_Desert : Boolean;
      end record;

   overriding function Object_Database
     (Feature : Root_Feature_Type)
      return Memor.Root_Database_Type'Class;

end Concorde.Features;
