private with Memor;
private with Memor.Database;

with Lui.Colors;

with Concorde.Objects;

package Concorde.Features is

   type Root_Feature_Type is
     new Concorde.Objects.Root_Localised_Object_Type
   with private;

   function Color
     (Feature : Root_Feature_Type'Class)
      return Lui.Colors.Color_Type;

   type Feature_Type is access constant Root_Feature_Type'Class;

   function Get (Id : String) return Feature_Type;

   function Ice return Feature_Type;
   function Desert return Feature_Type;

private

   type Root_Feature_Type is
     new Concorde.Objects.Root_Localised_Object_Type with
      record
         Color    : Lui.Colors.Color_Type;
         Is_Ice    : Boolean;
         Is_Desert : Boolean;
      end record;

   overriding function Object_Database
     (Feature : Root_Feature_Type)
      return Memor.Memor_Database;

   package Db is
     new Memor.Database
       ("feature", Root_Feature_Type, Feature_Type);

end Concorde.Features;
