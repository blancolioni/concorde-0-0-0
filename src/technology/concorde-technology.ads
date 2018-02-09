private with Memor.Database;

with Concorde.Objects;

package Concorde.Technology is

   type Root_Technology_Type is
     new Concorde.Objects.Root_Localised_Object_Type with private;

   type Technology_Type is access constant Root_Technology_Type'Class;

   function Exists (Name : String) return Boolean;
   function Get (Name : String) return Technology_Type
     with Pre => Exists (Name);

private

   type Root_Technology_Type is
     new Concorde.Objects.Root_Localised_Object_Type with
      record
         null;
      end record;

   overriding function Object_Database
     (Item : Root_Technology_Type)
      return Memor.Memor_Database;

   package Db is
     new Memor.Database
       ("technology", Root_Technology_Type, Technology_Type);

   overriding function Object_Database
     (Item : Root_Technology_Type)
      return Memor.Memor_Database
   is (Db.Get_Database);

end Concorde.Technology;
