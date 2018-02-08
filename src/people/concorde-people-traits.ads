private with Memor.Database;

with Concorde.Objects;

package Concorde.People.Traits is

   type Root_Trait_Type is
     new Concorde.Objects.Root_Localised_Object_Type with private;

   type Trait_Type is access constant Root_Trait_Type'Class;

   function Exists
     (S : String)
      return Boolean;

   function Get
     (S : String)
      return Trait_Type
     with Pre => Exists (S);

private

   type Root_Trait_Type is
     new Concorde.Objects.Root_Localised_Object_Type with
      record
         null;
      end record;

   overriding function Object_Database
     (Item : Root_Trait_Type)
      return Memor.Memor_Database;

   package Db is
     new Memor.Database
       ("trait", Root_Trait_Type, Trait_Type);

   overriding function Object_Database
     (Item : Root_Trait_Type)
      return Memor.Memor_Database
   is (Db.Get_Database);

   function Exists
     (S : String)
      return Boolean
   is (Db.Exists (S));

   function Get
     (S : String)
      return Trait_Type
   is (Db.Get (S));

end Concorde.People.Traits;
