private with Ada.Containers.Vectors;

private with Memor.Database;

with Concorde.Objects;

with Concorde.People.Attributes;

package Concorde.Offices is

   type Responsibility_Type is
     (Leader,
      Treasury,
      Army, Navy,
      Diplomacy);

   type Portfolio_Size_Range is new Positive;

   type Root_Office_Type is
     new Concorde.Objects.Root_Localised_Object_Type with private;

   type Office_Type is access constant Root_Office_Type'Class;

   function Effectiveness
     (Office    : Root_Office_Type'Class;
      Portfolio : Portfolio_Size_Range;
      Holder    : Concorde.People.Attributes.Has_Attributes'Class)
      return Unit_Real;

   function Score
     (Office    : Root_Office_Type'Class;
      Holder    : Concorde.People.Attributes.Has_Attributes'Class)
      return Natural;

   function Has_Responsibility
     (Office         : Root_Office_Type'Class;
      Responsibility : Responsibility_Type)
      return Boolean;

   function Exists (Name : String) return Boolean;

   function Get (Name : String) return Office_Type
     with Pre => Exists (Name);

   function Get (Responsibility : Responsibility_Type)
                 return Office_Type;

   procedure Scan_Offices
     (Process : not null access
        procedure (Office : Office_Type));

private

   type Responsibility_Array is array (Responsibility_Type) of Boolean;

   type Root_Office_Type is
     new Concorde.Objects.Root_Localised_Object_Type with
      record
         Responsibilities : Responsibility_Array;
         Effect           : Concorde.People.Attributes.Attribute_Container;
      end record;

   overriding function Object_Database
     (Item : Root_Office_Type)
      return Memor.Memor_Database;

   function Has_Responsibility
     (Office         : Root_Office_Type'Class;
      Responsibility : Responsibility_Type)
      return Boolean
   is (Office.Responsibilities (Responsibility));

   function Score
     (Office    : Root_Office_Type'Class;
      Holder    : Concorde.People.Attributes.Has_Attributes'Class)
      return Natural
   is (Office.Effect.Score (Holder));

   package Db is
     new Memor.Database
       ("office", Root_Office_Type, Office_Type);

   overriding function Object_Database
     (Item : Root_Office_Type)
      return Memor.Memor_Database
   is (Db.Get_Database);

   package Office_Vectors is
     new Ada.Containers.Vectors (Positive, Office_Type);

   Office_Vector : Office_Vectors.Vector;

   function Exists (Name : String) return Boolean
   is (Db.Exists (Name));

   function Get (Name : String) return Office_Type
   is (Db.Get (Name));

end Concorde.Offices;
