private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

private with Memor;
private with Memor.Database;

with Concorde.Agents;
with Concorde.Locations;
with Concorde.Managers;
with Concorde.Objects;
with Concorde.Regiments;
with Concorde.Trades;

limited with Concorde.Factions;
limited with Concorde.People.Individuals;

package Concorde.Armies is

   type Root_Army_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Objects.User_Named_Object_Interface
     and Concorde.Managers.Managed_Interface
   with private;

   function Faction
     (Army : Root_Army_Type'Class)
      return access constant Concorde.Factions.Root_Faction_Type'Class;

   function Commander
     (Army : Root_Army_Type'Class)
      return access constant
     Concorde.People.Individuals.Root_Individual_Type'Class;

   function Loyalty
     (Army : Root_Army_Type'Class)
      return Unit_Real;

   type Army_Type is access constant Root_Army_Type'Class;

   type Array_Of_Regiments is
     array (Positive range <>) of Concorde.Regiments.Regiment_Type;

   function Regiments
     (Army : Root_Army_Type'Class)
      return Array_Of_Regiments;

   procedure Add_Regiment
     (Army     : in out Root_Army_Type'Class;
      Regiment : Concorde.Regiments.Regiment_Type);

   procedure Remove_Regiment
     (Army     : in out Root_Army_Type'Class;
      Regiment : Concorde.Regiments.Regiment_Type);

   procedure Clear_Regiments
     (Army : in out Root_Army_Type'Class);

   procedure Update_Location
     (Army : Army_Type);

   type Updateable_Reference
     (Item : not null access Root_Army_Type'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Root_Army_Type'Class)
      return Updateable_Reference;

private

   package Regiment_Vectors is
     new Ada.Containers.Vectors (Positive, Concorde.Regiments.Regiment_Type,
                                 Concorde.Regiments."=");

   type Root_Army_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Objects.User_Named_Object_Interface
     and Concorde.Managers.Managed_Interface with
      record
         Army_Name : Ada.Strings.Unbounded.Unbounded_String;
         Faction   : access constant Concorde.Factions.Root_Faction_Type'Class;
         Commander : access constant
           Concorde.People.Individuals.Root_Individual_Type'Class;
         Location  : Concorde.Locations.Object_Location;
         Loyalty   : Unit_Real;
         Manager   : Concorde.Managers.Manager_Type;
         Regiments : Regiment_Vectors.Vector;
      end record;

   overriding function Object_Database
     (Item : Root_Army_Type)
      return Memor.Memor_Database;

   overriding function Class_Name
     (Army : Root_Army_Type) return String
   is ("army");

   overriding function Name
     (Army : Root_Army_Type)
      return String
   is (Ada.Strings.Unbounded.To_String (Army.Army_Name));

   overriding procedure Set_Name
     (Army : in out Root_Army_Type;
      New_Name   : String);

   overriding function Short_Name
     (Item : Root_Army_Type)
      return String
   is (Ada.Strings.Unbounded.To_String (Item.Army_Name));

   overriding function Variable_Reference
     (Army : not null access constant Root_Army_Type)
      return access Concorde.Agents.Root_Agent_Type'Class
   is (Army.Update.Item);

   overriding function Manager
     (Army  : Root_Army_Type)
      return Concorde.Managers.Manager_Type
   is (Army.Manager);

   overriding procedure Set_Manager
     (Army  : in out Root_Army_Type;
      Manager     : Concorde.Managers.Manager_Type);

   function Faction
     (Army : Root_Army_Type'Class)
      return access constant Concorde.Factions.Root_Faction_Type'Class
   is (Army.Faction);

   function Loyalty
     (Army : Root_Army_Type'Class)
      return Unit_Real
   is (Army.Loyalty);

   function Commander
     (Army : Root_Army_Type'Class)
      return access constant
     Concorde.People.Individuals.Root_Individual_Type'Class
   is (Army.Commander);

   package Db is
     new Memor.Database
       ("army", Root_Army_Type, Army_Type);

   overriding function Object_Database
     (Item : Root_Army_Type)
      return Memor.Memor_Database
   is (Db.Get_Database);

   type Updateable_Reference
     (Item : not null access Root_Army_Type'Class)
   is
      record
         Update : Db.Updateable_Reference (Item);
      end record;

end Concorde.Armies;
