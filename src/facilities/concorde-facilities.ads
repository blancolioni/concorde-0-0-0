private with Memor;

with Concorde.Money;
with Concorde.Quantities;

with Concorde.Commodities;
with Concorde.Objects;

package Concorde.Facilities is

   type Facility_Class is
     (Colony_Hub, Consulate, Corporate_HQ, Orbital_Dock,
      Factory, Resource_Generator, Service_Facility);

   type Facility_Flag is
     (Medical,
      Fitness,
      Entertainment,
      Education);

   type Facility_Capacity is range 0 .. 99_999;

   type Root_Facility_Type is
     new Concorde.Objects.Root_Named_Object_Type with private;

   function Class
     (Facility : Root_Facility_Type'Class)
      return Facility_Class;

   function Quality
     (Facility : Root_Facility_Type'Class)
      return Concorde.Commodities.Commodity_Quality;

   function Is_Set
     (Facility : Root_Facility_Type'Class;
      Flag     : Facility_Flag)
      return Boolean;

   function Is_Set
     (Facility : Root_Facility_Type'Class;
      Flag     : Concorde.Commodities.Commodity_Flag)
      return Boolean;

   function Base_Service_Charge
     (Facility : Root_Facility_Type'Class)
      return Concorde.Money.Price_Type;

   function Capacity
     (Facility : Root_Facility_Type'Class)
      return Facility_Capacity;

   function Input_Count
     (Facility : Root_Facility_Type'Class)
      return Natural;

   function Input_Commodity
     (Facility : Root_Facility_Type'Class;
      Index    : Positive)
      return Concorde.Commodities.Commodity_Type
     with Pre => Index <= Facility.Input_Count;

   function Input_Quantity
     (Facility : Root_Facility_Type'Class;
      Index    : Positive)
      return Concorde.Quantities.Quantity
     with Pre => Index <= Facility.Input_Count;

   function Has_Output
     (Facility : Root_Facility_Type'Class)
      return Boolean;

   function Output
     (Facility : Root_Facility_Type'Class)
      return Concorde.Commodities.Commodity_Type
     with Pre => Facility.Has_Output;

   type Facility_Type is access constant Root_Facility_Type'Class;

   function Get (Name : String) return Facility_Type;

private

   type Array_Of_Flags is array (Facility_Flag) of Boolean;

   type Input_Record is
      record
         Commodity : Concorde.Commodities.Commodity_Type;
         Quantity  : Concorde.Quantities.Quantity;
      end record;

   type Array_Of_Inputs is array (Positive range <>) of Input_Record;

   type Root_Facility_Type is
     new Concorde.Objects.Root_Named_Object_Type with
      record
         Tag                 : access String;
         Class               : Facility_Class;
         Template            : Boolean;
         Flags               : Array_Of_Flags;
         Quality             : Concorde.Commodities.Commodity_Quality;
         Power               : Concorde.Quantities.Quantity;
         Capacity            : Facility_Capacity;
         Commodity_Flags     : Concorde.Commodities.Array_Of_Flags;
         Inputs              : access Array_Of_Inputs;
         Output              : Concorde.Commodities.Commodity_Type;
         Base_Service_Charge : Concorde.Money.Price_Type;
      end record;

   overriding function Object_Database
     (Item : Root_Facility_Type)
      return Memor.Root_Database_Type'Class;

   overriding function Identifier
     (Item : Root_Facility_Type)
      return String
   is (Item.Tag.all);

end Concorde.Facilities;
