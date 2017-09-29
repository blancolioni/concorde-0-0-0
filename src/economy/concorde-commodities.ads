private with Ada.Containers.Vectors;

private with Memor;
private with Memor.Database;
private with Memor.Element_Vectors;

with Concorde.Money;
with Concorde.Quantities;

with Concorde.Objects;

package Concorde.Commodities is

   use type Concorde.Quantities.Quantity_Type;

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
     new Concorde.Objects.Root_Localised_Object_Type with private;

   function Class
     (Commodity : Root_Commodity_Type'Class)
      return Commodity_Class;

   function Quality
     (Commodity : Root_Commodity_Type'Class)
      return Commodity_Quality;

   function Unit_Mass
     (Commodity : Root_Commodity_Type'Class)
      return Non_Negative_Real;

   function Base_Price
     (Commodity : Root_Commodity_Type'Class)
      return Concorde.Money.Price_Type;

   function Is_Set
     (Commodity : Root_Commodity_Type'Class;
      Flag      : Commodity_Flag)
      return Boolean;

   type Commodity_Type is access constant Root_Commodity_Type'Class;

   function Get (Name : String) return Commodity_Type;

   procedure Scan (Process : not null access
                     procedure (Commodity : Commodity_Type));

   type Array_Of_Commodities is array (Positive range <>) of Commodity_Type;

   function Get (Class : Commodity_Class) return Array_Of_Commodities;

   function Get (Flag : Commodity_Flag) return Array_Of_Commodities;

   function Get (Class   : Commodity_Class;
                 Quality : Commodity_Quality)
                 return Array_Of_Commodities;

   function All_Commodities return Array_Of_Commodities;

   type Stock_Interface is limited interface;

   function Total_Quantity
     (Stock    : Stock_Interface'Class)
      return Concorde.Quantities.Quantity_Type;

   function Total_Mass
     (Stock    : Stock_Interface'Class)
      return Non_Negative_Real;

   function Maximum_Quantity
     (Stock : Stock_Interface)
      return Concorde.Quantities.Quantity_Type
      is abstract;

   function Available_Quantity
     (Stock    : Stock_Interface'Class)
      return Concorde.Quantities.Quantity_Type
   is (Stock.Maximum_Quantity - Stock.Total_Quantity);

   function Get_Quantity
     (Stock : Stock_Interface;
      Item  : Commodity_Type)
      return Concorde.Quantities.Quantity_Type
      is abstract;

   function Get_Value
     (Stock : Stock_Interface;
      Item  : Commodity_Type)
      return Concorde.Money.Money_Type
      is abstract;

   function Get_Average_Price
     (Stock : Stock_Interface'Class;
      Item  : Commodity_Type)
      return Concorde.Money.Price_Type
   is (Concorde.Money.Price
       (Stock.Get_Value (Item), Stock.Get_Quantity (Item)));

   procedure Set_Quantity
     (Stock    : in out Stock_Interface;
      Item     : Commodity_Type;
      Quantity : Concorde.Quantities.Quantity_Type;
      Value    : Concorde.Money.Money_Type)
   is abstract;
--       with Pre'Class =>
--         Stock.Total_Quantity + Quantity
--           - Stock_Interface'Class (Stock).Get_Quantity (Item)
--       <= Stock.Maximum_Quantity;

   procedure Scan_Stock
     (Stock : Stock_Interface;
      Process : not null access
        procedure (Commodity : Commodity_Type))
   is abstract;

   procedure Clear_Stock
     (Stock : in out Stock_Interface)
   is abstract;

   procedure Add_Quantity
     (Stock    : in out Stock_Interface'Class;
      Item     : Commodity_Type;
      Quantity : Concorde.Quantities.Quantity_Type;
      Value    : Concorde.Money.Money_Type)
     with Pre => Stock.Total_Quantity + Quantity
       <= Stock.Maximum_Quantity;

   procedure Remove_Quantity
     (Stock    : in out Stock_Interface'Class;
      Item     : Commodity_Type;
      Quantity : Concorde.Quantities.Quantity_Type;
      Earn     : Concorde.Money.Money_Type)
     with Pre => Stock.Get_Quantity (Item) >= Quantity;

   procedure Remove_Quantity
     (Stock    : in out Stock_Interface'Class;
      Item     : Commodity_Type;
      Quantity : Concorde.Quantities.Quantity_Type)
     with Pre => Stock.Get_Quantity (Item) >= Quantity;

   function Total_Value
     (Stock    : in out Stock_Interface'Class)
      return Concorde.Money.Money_Type;

   type Root_Stock_Type is new Stock_Interface with private;

   procedure Create_Stock
     (Stock   : in out Root_Stock_Type'Class;
      Maximum : Concorde.Quantities.Quantity_Type);

private

   type Root_Commodity_Type is
     new Concorde.Objects.Root_Localised_Object_Type with
      record
         Class      : Commodity_Class;
         Flags      : Array_Of_Flags;
         Base_Price : Concorde.Money.Price_Type;
         Mass       : Non_Negative_Real;
         Quality    : Commodity_Quality;
      end record;

   overriding function Object_Database
     (Item : Root_Commodity_Type)
      return Memor.Memor_Database;

   type Stock_Entry is
      record
         Quantity : Concorde.Quantities.Quantity_Type;
         Value    : Concorde.Money.Money_Type;
      end record;

   package Stock_Vectors is
     new Memor.Element_Vectors
       (Concorde.Commodities.Root_Commodity_Type,
        Stock_Entry,
        (Concorde.Quantities.Zero, Concorde.Money.Zero));

   type Root_Stock_Type is new Stock_Interface with
      record
         Maximum : Quantities.Quantity_Type   := Quantities.Zero;
         Vector  : Stock_Vectors.Vector;
      end record;

   overriding function Maximum_Quantity
     (Stock : Root_Stock_Type)
      return Quantities.Quantity_Type
   is (Stock.Maximum);

   overriding function Get_Quantity
     (Stock : Root_Stock_Type;
      Item  : Commodity_Type)
      return Concorde.Quantities.Quantity_Type
   is (Stock.Vector.Element (Item).Quantity);

   overriding function Get_Value
     (Stock : Root_Stock_Type;
      Item  : Commodity_Type)
      return Concorde.Money.Money_Type
   is (Stock.Vector.Element (Item).Value);

   overriding procedure Set_Quantity
     (Stock    : in out Root_Stock_Type;
      Item     : Commodity_Type;
      Quantity : Concorde.Quantities.Quantity_Type;
      Value    : Concorde.Money.Money_Type);

   overriding procedure Clear_Stock
     (Stock : in out Root_Stock_Type);

   overriding procedure Scan_Stock
     (Stock   : Root_Stock_Type;
      Process : not null access
        procedure (Commodity : Commodity_Type));

   package Db is
     new Memor.Database
       ("commodity", Root_Commodity_Type, Commodity_Type);

   package Commodity_Vectors is
     new Ada.Containers.Vectors (Positive, Commodity_Type);

   Commodity_Vector : Commodity_Vectors.Vector;

end Concorde.Commodities;
