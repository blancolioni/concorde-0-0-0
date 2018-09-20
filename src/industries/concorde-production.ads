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

   procedure Execute
     (Production : Root_Production_Type'Class;
      Stock      : in out Concorde.Commodities.Stock_Interface'Class;
      Size       : Non_Negative_Real;
      Cost       : out Concorde.Money.Money_Type);

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
