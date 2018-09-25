private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;
private with Memor.Database;

with Memor;

with Concorde.Money;
with Concorde.Quantities;

with Concorde.Objects;

with Concorde.Commodities;

with Concorde.Network.Nodes;

package Concorde.Production is

   type Production_Environment_Interface is limited interface;

   procedure Grow_Food
     (Environment     : in out Production_Environment_Interface;
      Food_Production : Non_Negative_Real;
      Grown           : out Non_Negative_Real)
   is abstract;

   procedure Mine_Resource
     (Environment     : in out Production_Environment_Interface;
      Resource        : Concorde.Commodities.Commodity_Type;
      Mine_Production : Non_Negative_Real;
      Mined           : out Non_Negative_Real)
   is abstract;

   type Root_Production_Type is
     new Concorde.Objects.Root_Localised_Object_Type
   with private;

   type Production_Type is access constant Root_Production_Type'Class;

   function Input_Quantity
     (Production : Root_Production_Type'Class;
      Commodity  : Concorde.Commodities.Commodity_Type;
      Size       : Non_Negative_Real)
      return Concorde.Quantities.Quantity_Type;

   function Relative_Input_Cost
     (Production : Root_Production_Type'Class;
      Commodity  : Concorde.Commodities.Commodity_Type)
      return Unit_Real;

   function Is_Output
     (Production : Root_Production_Type'Class;
      Commodity  : Concorde.Commodities.Commodity_Type)
      return Boolean;

   function Outputs
     (Production : Root_Production_Type'Class)
      return Concorde.Commodities.Array_Of_Commodities;

   procedure Execute
     (Production  : Root_Production_Type'Class;
      Environment : in out Production_Environment_Interface'Class;
      Stock       : in out Concorde.Commodities.Stock_Interface'Class;
      Size        : Non_Negative_Real;
      Cost        : out Concorde.Money.Money_Type);

   function Minimum_Size
     (Production : Root_Production_Type'Class;
      To_Produce : Concorde.Commodities.Stock_Interface'Class)
      return Non_Negative_Real;

   function Exists (Name : String) return Boolean;
   function Get (Name : String) return Production_Type
     with Pre => Exists (Name);

private

   type Production_Commodity is
      record
         Commodity         : Concorde.Commodities.Commodity_Type;
         Relative_Quantity : Non_Negative_Real;
         Consumption       : Unit_Real;
      end record;

   package Production_Commodity_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Production_Commodity);

   type Root_Production_Type is
     new Concorde.Objects.Root_Localised_Object_Type with
      record
         Inputs  : Production_Commodity_Lists.List;
         Outputs : Production_Commodity_Lists.List;
      end record;

   overriding function Object_Database
     (Item : Root_Production_Type)
      return Memor.Memor_Database;

   package Db is
     new Memor.Database
       ("production", Root_Production_Type, Production_Type);

   overriding function Object_Database
     (Item : Root_Production_Type)
      return Memor.Memor_Database
   is (Db.Get_Database);

   package Production_Type_Vectors is
     new Ada.Containers.Vectors (Positive, Production_Type);

   Vector : Production_Type_Vectors.Vector;

end Concorde.Production;
