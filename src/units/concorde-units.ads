private with Ada.Strings.Unbounded;

private with Memor.Database;

with Concorde.Objects;

with Concorde.Commodities;
with Concorde.Construction;

package Concorde.Units is

   type Movement_Category is (Air, Land, Sea);

   type Root_Unit_Type is
     new Concorde.Objects.Root_Localised_Object_Type
     and Concorde.Construction.Constructed_Interface
   with private;

   overriding function Construction_Stock
     (Unit : Root_Unit_Type)
      return Concorde.Commodities.Stock_Interface'Class;

   overriding function Maintenance_Stock
     (Unit : Root_Unit_Type)
      return Concorde.Commodities.Stock_Interface'Class;

   type Unit_Type is access constant Root_Unit_Type'Class;

   function Movement
     (Unit : Root_Unit_Type)
      return Movement_Category;

   function Speed
     (Unit : Root_Unit_Type)
      return Natural;

   function Recon
     (Unit : Root_Unit_Type)
      return Natural;

   function Priority
     (Unit : Root_Unit_Type)
      return Natural;

   function Image_Resource
     (Unit : Root_Unit_Type)
      return String;

   function Exists (Id : String) return Boolean;

   function Get (Id : String) return Unit_Type
     with Pre => Exists (Id);

private

   type Weapon_Record is
      record
         Strength : Natural := 0;
      end record;

   type Root_Unit_Type is
     new Concorde.Objects.Root_Localised_Object_Type
       and Concorde.Construction.Constructed_Interface with
      record
         Movement       : Movement_Category;
         Speed          : Natural := 0;
         Recon          : Natural := 0;
         Cargo          : Natural := 0;
         Can_Be_Cargo   : Boolean := False;
         Combat         : Boolean := False;
         Priority       : Natural := 0;
         Construct      : Concorde.Construction.Constructed_Record;
         Image_Resource : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Object_Database
     (Item : Root_Unit_Type)
      return Memor.Memor_Database;

   overriding function Construction_Stock
     (Unit : Root_Unit_Type)
      return Concorde.Commodities.Stock_Interface'Class
   is (Unit.Construct.Construction_Stock);

   overriding function Maintenance_Stock
     (Unit : Root_Unit_Type)
      return Concorde.Commodities.Stock_Interface'Class
   is (Unit.Construct.Maintenance_Stock);

   function Movement
     (Unit : Root_Unit_Type)
      return Movement_Category
   is (Unit.Movement);

   function Speed
     (Unit : Root_Unit_Type)
      return Natural
   is (Unit.Speed);

   function Recon
     (Unit : Root_Unit_Type)
      return Natural
   is (Unit.Recon);

   function Priority
     (Unit : Root_Unit_Type)
      return Natural
   is (Unit.Priority);

   function Image_Resource
     (Unit : Root_Unit_Type)
      return String
   is (Ada.Strings.Unbounded.To_String (Unit.Image_Resource));

   package Db is
     new Memor.Database
       (Class_Name        => "unit",
        Element_Type      => Root_Unit_Type,
        Element_Reference => Unit_Type);

   overriding function Object_Database
     (Item : Root_Unit_Type)
      return Memor.Memor_Database
   is (Db.Get_Database);

   function Exists (Id : String) return Boolean
   is (Db.Exists (Id));

   function Get (Id : String) return Unit_Type
   is (Db.Get (Id));

end Concorde.Units;
