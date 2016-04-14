private with Memor;
private with Memor.Element_Vectors;

with Concorde.Money;
with Concorde.Quantities;

with Concorde.Objects;

package Concorde.Commodities is

   type Commodity_Class is
     (Resource, Consumer, Industrial, Building_Component, Skill, Service);

   type Commodity_Flag is
     (Organic, Mineral, Metal, Fissile, Fuel, Gas, Liquid,
      Food, Drink, Intoxicant, Clothing,
      Alloy, Ceramic, Electronic, Plastic,
      Virtual, Power, Generator);

   type Array_Of_Flags is array (Commodity_Flag) of Boolean;

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

   type Stock_Interface is limited interface;

   function Get_Quantity
     (Stock : Stock_Interface;
      Item  : Commodity_Type)
      return Concorde.Quantities.Quantity
      is abstract;

   procedure Set_Quantity
     (Stock    : in out Stock_Interface;
      Item     : Commodity_Type;
      Quantity : Concorde.Quantities.Quantity)
   is abstract;

   procedure Add_Quantity
     (Stock    : in out Stock_Interface'Class;
      Item     : Commodity_Type;
      Quantity : Concorde.Quantities.Quantity);

   procedure Remove_Quantity
     (Stock    : in out Stock_Interface'Class;
      Item     : Commodity_Type;
      Quantity : Concorde.Quantities.Quantity);

   function Total_Quantity
     (Stock    : in out Stock_Interface'Class)
      return Concorde.Quantities.Quantity;

   type Root_Stock_Type is new Stock_Interface with private;

private

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

   package Stock_Vectors is
     new Memor.Element_Vectors (Concorde.Quantities.Quantity,
                                Concorde.Quantities.Zero,
                                Concorde.Quantities."=");

   type Root_Stock_Type is new Stock_Interface with
      record
         Vector : Stock_Vectors.Vector;
      end record;

   overriding function Get_Quantity
     (Stock : Root_Stock_Type;
      Item  : Commodity_Type)
      return Concorde.Quantities.Quantity
   is (Stock.Vector.Element (Item.Reference));

   overriding procedure Set_Quantity
     (Stock    : in out Root_Stock_Type;
      Item     : Commodity_Type;
      Quantity : Concorde.Quantities.Quantity);

end Concorde.Commodities;
