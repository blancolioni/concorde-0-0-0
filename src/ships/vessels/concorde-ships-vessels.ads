private with Ada.Containers.Vectors;
private with Memor.Database;

with Concorde.Commodities;
with Concorde.Ships.Designs;

package Concorde.Ships.Vessels is

   type Root_Vessel_Type is
     new Root_Ship_Type with private;

   type Updateable_Reference
     (Item : not null access Root_Vessel_Type'Class)
   is private with Implicit_Dereference => Item;

   function Update_Vessel
     (Item : not null access constant Root_Vessel_Type'Class)
      return Updateable_Reference;

   function Get_Design
     (Vessel : Root_Vessel_Type'Class)
      return Concorde.Ships.Designs.Design_Type;

   type Vessel_Type is access constant Root_Vessel_Type'Class;

private

   type Ship_Module_Record is
      record
         Stock     : Concorde.Commodities.Root_Stock_Type;
         Heat      : Non_Negative_Real;
         Condition : Unit_Real;
      end record;

   package Ship_Module_Vectors is
     new Ada.Containers.Vectors
       (Concorde.Ships.Designs.Module_Index, Ship_Module_Record);

   type Root_Vessel_Type is
     new Root_Ship_Type with
      record
         Design   : Concorde.Ships.Designs.Design_Type;
         Modules  : Ship_Module_Vectors.Vector;
         Is_Alive : Boolean;
      end record;

   overriding procedure Update_Agent
     (Vessel         : not null access constant Root_Vessel_Type;
      Perform_Update : not null access
        procedure (Agent : in out Concorde.Agents.Root_Agent_Type'Class));

   overriding function Class_Name
     (Ship : Root_Vessel_Type)
      return String
   is ("vessel");

   overriding function Variable_Reference
     (Vessel : not null access constant Root_Vessel_Type)
      return access Concorde.Agents.Root_Agent_Type'Class;

   overriding function Alive
     (Ship : Root_Vessel_Type)
      return Boolean
   is (Ship.Is_Alive);

   overriding function Classification
     (Ship : Root_Vessel_Type)
      return Ship_Classification
   is (Ship.Design.Get_Classification);

   overriding function Maximum_Thrust
     (Ship : Root_Vessel_Type)
      return Non_Negative_Real;

   overriding function Current_Mass
     (Ship : Root_Vessel_Type)
      return Non_Negative_Real;

   overriding function Cargo_Capacity
     (Ship      : Root_Vessel_Type)
      return Concorde.Quantities.Quantity_Type;

   overriding function Available_Capacity
     (Ship      : Root_Vessel_Type;
      Commodity : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Concorde.Quantities.Quantity_Type;

   overriding function Update
     (Vessel : not null access constant Root_Vessel_Type)
      return access Root_Ship_Type'Class;

   overriding function Object_Database
     (Item : Root_Vessel_Type)
      return Memor.Memor_Database;

   function Get_Design
     (Vessel : Root_Vessel_Type'Class)
      return Concorde.Ships.Designs.Design_Type
   is (Vessel.Design);

   package Db is
     new Memor.Database
       ("ship-vessel", Root_Vessel_Type, Vessel_Type);

   overriding function Cargo_Capacity
     (Ship      : Root_Vessel_Type)
      return Concorde.Quantities.Quantity_Type
   is (Ship.Design.Cargo_Capacity);

   overriding function Object_Database
     (Item : Root_Vessel_Type)
      return Memor.Memor_Database
   is (Db.Get_Database);

   overriding function Update
     (Vessel : not null access constant Root_Vessel_Type)
      return access Root_Ship_Type'Class
   is (Root_Ship_Type'Class (Vessel.Variable_Reference.all)'Access);

   type Updateable_Reference
     (Item : not null access Root_Vessel_Type'Class)
   is
      record
         Update : Db.Updateable_Reference (Item);
      end record;

end Concorde.Ships.Vessels;
