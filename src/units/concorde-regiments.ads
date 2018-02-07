private with Memor.Database;

with Concorde.Locations;
with Concorde.Objects;
with Concorde.People.Pops;
with Concorde.Units;

package Concorde.Regiments is

   type Root_Regiment_Type is
     new Concorde.Objects.Root_User_Named_Object_Type
   with private;

   type Regiment_Type is
     access constant Root_Regiment_Type'Class;

   function Unit
     (Regiment : Root_Regiment_Type'Class)
      return Concorde.Units.Unit_Type;

private

   type Root_Regiment_Type is
     new Concorde.Objects.Root_User_Named_Object_Type with
      record
         Unit         : Concorde.Units.Unit_Type;
         Pop          : Concorde.People.Pops.Pop_Type;
         Size         : Natural;
         Morale       : Unit_Real;
         Organisation : Unit_Real;
         Loyalty      : Unit_Real;
      end record;

   overriding function Object_Database
     (Item : Root_Regiment_Type)
      return Memor.Memor_Database;

   function Unit
     (Regiment : Root_Regiment_Type'Class)
      return Concorde.Units.Unit_Type
   is (Regiment.Unit);

   package Db is
     new Memor.Database
       (Class_Name        => "regiment",
        Element_Type      => Root_Regiment_Type,
        Element_Reference => Regiment_Type);

   overriding function Object_Database
     (Item : Root_Regiment_Type)
      return Memor.Memor_Database
   is (Db.Get_Database);

end Concorde.Regiments;
