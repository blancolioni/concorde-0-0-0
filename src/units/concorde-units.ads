private with Ada.Strings.Unbounded;

private with Memor.Database;

with Concorde.Objects;

with Concorde.Commodities;

package Concorde.Units is

   type Movement_Category is
     (Air, Crawler, Foot, Hover, Tread, Wheel);

   type Weapon_Category is
     (Indirect, Air, Direct, Close);

   type Root_Unit_Type is
     new Concorde.Objects.Root_Localised_Object_Type with private;

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

   function Stealth
     (Unit : Root_Unit_Type)
      return Natural;

   function Armour
     (Unit : Root_Unit_Type)
      return Natural;

   function Has_Attack
     (Unit   : Root_Unit_Type;
      Weapon : Weapon_Category)
      return Boolean;

   function Strength
     (Unit   : Root_Unit_Type;
      Weapon : Weapon_Category)
      return Positive
     with Pre => Unit.Has_Attack (Weapon);

   function Rank
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

   type Weapon_Array is array (Weapon_Category) of Weapon_Record;

   type Root_Unit_Type is
     new Concorde.Objects.Root_Localised_Object_Type with
      record
         Movement       : Movement_Category;
         Speed          : Natural := 0;
         Recon          : Natural := 0;
         Stealth        : Natural := 0;
         Armour         : Natural := 0;
         Weapons        : Weapon_Array := (others => (Strength => 0));
         Cargo          : Natural := 0;
         Can_Be_Cargo   : Boolean := False;
         Combat         : Boolean := False;
         Rank           : Natural := 0;
         Turns_To_Build : Natural := 0;
         Image_Resource : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Object_Database
     (Item : Root_Unit_Type)
      return Memor.Memor_Database;

   function Movement
     (Unit : Root_Unit_Type)
      return Movement_Category
   is (Unit.Movement);

   function Speed
     (Unit : Root_Unit_Type)
      return Natural
   is (Unit.Speed);

   function Stealth
     (Unit : Root_Unit_Type)
      return Natural
   is (Unit.Stealth);

   function Recon
     (Unit : Root_Unit_Type)
      return Natural
   is (Unit.Recon);

   function Armour
     (Unit : Root_Unit_Type)
      return Natural
   is (Unit.Armour);

   function Rank
     (Unit : Root_Unit_Type)
      return Natural
   is (Unit.Rank);

   function Has_Attack
     (Unit   : Root_Unit_Type;
      Weapon : Weapon_Category)
      return Boolean
   is (Unit.Weapons (Weapon).Strength > 0);

   function Strength
     (Unit   : Root_Unit_Type;
      Weapon : Weapon_Category)
      return Positive
   is (Unit.Weapons (Weapon).Strength);

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
