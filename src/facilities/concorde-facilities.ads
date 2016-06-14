private with Memor;

with Concorde.Money;
with Concorde.Quantities;

with Concorde.Commodities;
with Concorde.Objects;

with Concorde.People.Skills;

package Concorde.Facilities is

   type Facility_Class is
     (Colony_Hub, Port, Consulate, Corporate_HQ, Orbital_Dock,
      Factory, Resource_Generator, Farm, Service_Facility);

   type Facility_Flag is
     (Medical,
      Fitness,
      Entertainment,
      Education);

   type Facility_Capacity is range 0 .. 9_999_999;

   type Root_Facility_Type is
     new Concorde.Objects.Root_Localised_Object_Type with private;

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

   function Base_Service_Charge
     (Facility : Root_Facility_Type'Class)
      return Concorde.Money.Price_Type;

   function Capacity
     (Facility : Root_Facility_Type'Class)
      return Facility_Capacity;

   function Capacity_Quantity
     (Facility : Root_Facility_Type'Class)
      return Concorde.Quantities.Quantity
   is (Quantities.To_Quantity (Real (Facility.Capacity)));

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

   function Worker_Count
     (Facility : Root_Facility_Type'Class)
      return Natural;

   function Worker_Skill
     (Facility : Root_Facility_Type'Class;
      Index    : Positive)
      return Concorde.People.Skills.Pop_Skill
     with Pre => Index <= Facility.Worker_Count;

--     function Worker_Skill
--       (Facility : Root_Facility_Type'Class;
--        Index    : Positive)
--        return Concorde.Commodities.Commodity_Type
--     is (Facility.Worker_Skill (Index).Commodity)
--     with Pre => Index <= Facility.Worker_Count;

   function Worker_Quantity
     (Facility : Root_Facility_Type'Class;
      Index    : Positive)
      return Concorde.Quantities.Quantity
     with Pre => Index <= Facility.Worker_Count;

   function Is_Resource_Generator
     (Facility : Root_Facility_Type'Class)
      return Boolean
   is (Facility.Class = Resource_Generator);

   function Is_Farm
     (Facility : Root_Facility_Type'Class)
      return Boolean
   is (Facility.Class = Farm);

   function Has_Output
     (Facility : Root_Facility_Type'Class)
      return Boolean;

   function Output
     (Facility : Root_Facility_Type'Class)
      return Concorde.Commodities.Commodity_Type
     with Pre => Facility.Has_Output;

   function Can_Produce
     (Facility  : Root_Facility_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Boolean;

   type Facility_Type is access constant Root_Facility_Type'Class;

   function Get (Name : String) return Facility_Type;

   type Array_Of_Facilities is
     array (Positive range <>) of Facility_Type;

   function Get_By_Production
     (Output : Concorde.Commodities.Commodity_Type)
      return Array_Of_Facilities;

   function Get_By_Production
     (Output : Concorde.Commodities.Root_Commodity_Type'Class)
      return Array_Of_Facilities;

   function Get_By_Class
     (Class : Facility_Class)
      return Array_Of_Facilities;

   function Colony_Hub return Facility_Type;

   function Resource_Generator
     (Resource : Concorde.Commodities.Commodity_Type)
      return Facility_Type
     with Pre => Concorde.Commodities."="
       (Resource.Class, Concorde.Commodities.Resource);

private

   type Array_Of_Flags is array (Facility_Flag) of Boolean;

   type Input_Record is
      record
         Commodity : Concorde.Commodities.Commodity_Type;
         Quantity  : Concorde.Quantities.Quantity;
      end record;

   type Array_Of_Inputs is array (Positive range <>) of Input_Record;

   type Worker_Record is
      record
         Skill     : Concorde.People.Skills.Pop_Skill;
         Quantity  : Concorde.Quantities.Quantity;
      end record;

   type Array_Of_Workers is
     array (Positive range <>) of Worker_Record;

   type Root_Facility_Type is
     new Concorde.Objects.Root_Localised_Object_Type with
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
         Workers             : access Array_Of_Workers;
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
