private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;

private with Memor.Database;

with Concorde.Objects;
with Concorde.Ships.Modules;

with Concorde.Vectors;

package Concorde.Ships.Designs is

   Max_Ship_Modules : constant := 100;
   type Module_Count is range 0 .. Max_Ship_Modules;
   subtype Module_Index is Module_Count range 1 .. Module_Count'Last;

   type Root_Design_Type is
     new Concorde.Objects.Root_User_Named_Object_Type with private;

   function Get_Classification
     (Design : Root_Design_Type'Class)
      return Ship_Classification;

   function Get_Module_Count
     (Design : Root_Design_Type'Class)
      return Module_Count;

   function Get_Module
     (Design : Root_Design_Type'Class;
      Index  : Module_Index)
      return Concorde.Ships.Modules.Module_Type;

   function Cargo_Capacity
     (Design : Root_Design_Type'Class)
      return Concorde.Quantities.Quantity_Type;

   procedure Get_Fuel_Requirements
     (Design       : Root_Design_Type'Class;
      Requirements : in out Concorde.Commodities.Stock_Interface'Class);

   type Design_Type is access constant Root_Design_Type'Class;

   function Exists (Name : String) return Boolean;

   function Get (Name : String) return Design_Type
     with Pre => Exists (Name);

private

   type Installed_Module_Record is
      record
         Module      : Concorde.Ships.Modules.Module_Type;
      end record;

   package Installed_Module_Vectors is
     new Ada.Containers.Vectors (Module_Index, Installed_Module_Record);

   type Module_Attachment_Record is
      record
         From_Module : Module_Index;
         To_Module   : Module_Index;
         From_Attach : Concorde.Vectors.Vector_3;
         To_Attach   : Concorde.Vectors.Vector_3;
      end record;

   package Module_Attachment_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Module_Attachment_Record);

   type Root_Design_Type is
     new Concorde.Objects.Root_User_Named_Object_Type with
      record
         Identifier        : Ada.Strings.Unbounded.Unbounded_String;
         Installed_Modules : Installed_Module_Vectors.Vector;
         Attached_Modules  : Module_Attachment_Lists.List;
         Classification    : Ship_Classification;
      end record;

   overriding function Object_Database
     (Item : Root_Design_Type)
      return Memor.Memor_Database;

   overriding function Identifier
     (Item : Root_Design_Type)
      return String;

   package Db is
     new Memor.Database
       ("ship-design", Root_Design_Type, Design_Type);

   overriding function Object_Database
     (Item : Root_Design_Type)
      return Memor.Memor_Database
   is (Db.Get_Database);

   function Get_Classification
     (Design : Root_Design_Type'Class)
      return Ship_Classification
   is (Design.Classification);

   function Get_Module_Count
     (Design : Root_Design_Type'Class)
      return Module_Count
   is (Design.Installed_Modules.Last_Index);

   function Get_Module
     (Design : Root_Design_Type'Class;
      Index  : Module_Index)
      return Concorde.Ships.Modules.Module_Type
   is (Design.Installed_Modules.Element (Index).Module);

   function Exists (Name : String) return Boolean
   is (Db.Exists (Name));

   function Get (Name : String) return Design_Type
   is (Db.Get (Name));

end Concorde.Ships.Designs;
