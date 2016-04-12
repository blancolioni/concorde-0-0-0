private with Memor;

with Concorde.Money;
with Concorde.Objects;

package Concorde.Commodities is

   type Commodity_Class is
     (Resource, Consumer, Industrial, Building_Component);

   type Commodity_Flag is
     (Organic, Mineral, Metal, Fissile, Fuel, Gas, Liquid,
      Food, Drink, Intoxicant, Clothing,
      Alloy, Ceramic, Electronic, Plastic,
      Virtual, Power, Generator);

   type Commodity_Quality is (High, Middle, Low);

   type Root_Commodity_Type is
     new Concorde.Objects.Root_Named_Object_Type with private;

   function Class
     (Commodity : Root_Commodity_Type'Class)
      return Commodity_Class;

   function Quality
     (Commodity : Root_Commodity_Type'Class)
      return Commodity_Quality;

   function Is_Set
     (Commodity : Root_Commodity_Type'Class;
      Flag      : Commodity_Flag)
      return Boolean;

   type Commodity_Type is access constant Root_Commodity_Type'Class;

   function Get (Name : String) return Commodity_Type;

   type Array_Of_Commodities is array (Positive range <>) of Commodity_Type;

   function Get (Class : Commodity_Class) return Array_Of_Commodities;

private

   type Array_Of_Flags is array (Commodity_Flag) of Boolean;

   type Root_Commodity_Type is
     new Concorde.Objects.Root_Named_Object_Type with
      record
         Tag        : access String;
         Class      : Commodity_Class;
         Flags      : Array_Of_Flags;
         Base_Price : Concorde.Money.Price_Type;
         Mass       : Non_Negative_Real;
         Quality    : Commodity_Quality;
      end record;

   overriding function Object_Database
     (Item : Root_Commodity_Type)
      return Memor.Root_Database_Type'Class;

   overriding function Identifier
     (Item : Root_Commodity_Type)
      return String
   is (Item.Tag.all);

end Concorde.Commodities;
