private with Memor;

with Concorde.Objects;

package Concorde.Features is

   type Root_Feature_Type is
     new Concorde.Objects.Root_Named_Object_Type
   with private;

   type Feature_Type is access constant Root_Feature_Type'Class;

   function Ice return Feature_Type;

private

   type Root_Feature_Type is
     new Concorde.Objects.Root_Named_Object_Type with
      record
         Is_Ice : Boolean;
      end record;

   overriding function Object_Database
     (Feature : Root_Feature_Type)
      return Memor.Root_Database_Type'Class;

end Concorde.Features;
