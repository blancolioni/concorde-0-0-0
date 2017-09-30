private with Ada.Containers.Doubly_Linked_Lists;

private with Memor;
private with Memor.Database;

with Concorde.Agents;
with Concorde.Trades;

with Concorde.Facilities;

with Concorde.Locations;

limited with Concorde.People.Individuals;
with Concorde.People.Skills;

with Concorde.People.Pops;

with Concorde.Dates;
with Concorde.Money;
with Concorde.Quantities;
with Concorde.Commodities;

package Concorde.Installations is

   type Root_Installation_Type is
     new Concorde.Agents.Root_Agent_Type
   with private;

   function Is_Colony_Hub
     (Installation : Root_Installation_Type'Class)
      return Boolean;

   function Is_Port
     (Installation : Root_Installation_Type'Class)
      return Boolean;

   function Facility
     (Installation : Root_Installation_Type'Class)
      return Concorde.Facilities.Facility_Type;

   function Owner
     (Installation : Root_Installation_Type'Class)
      return access constant Concorde.Agents.Root_Agent_Type'Class;

   function Has_Manager
     (Installation : Root_Installation_Type'Class)
      return Boolean;

   function Manager
     (Installation : Root_Installation_Type'Class)
      return access constant
     Concorde.People.Individuals.Root_Individual_Type'Class
       with Pre => Installation.Has_Manager;

   procedure Set_Manager
     (Installation : in out Root_Installation_Type'Class;
      Manager      : not null access constant
        Concorde.People.Individuals.Root_Individual_Type'Class)
       with Post => Installation.Has_Manager;

   procedure Remove_Manager
     (Installation : in out Root_Installation_Type'Class)
       with Pre => Installation.Has_Manager,
         Post => not Installation.Has_Manager;

   procedure Pay_Workers
     (Installation : in out Root_Installation_Type'Class);

   procedure Execute_Production
     (Installation : in out Root_Installation_Type'Class);

   procedure Add_Trade_Offers
     (Item   : not null access constant Root_Installation_Type);

   type Installation_Type is access constant Root_Installation_Type'Class;

   type Updateable_Reference
     (Item : not null access Root_Installation_Type'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Root_Installation_Type'Class)
      return Updateable_Reference;

private

   type Queued_Production_Element is
      record
         Done   : Concorde.Dates.Date_Type;
         Size   : Concorde.Quantities.Quantity_Type;
         Output : Concorde.Commodities.Commodity_Type;
      end record;

   package Queued_Production_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Queued_Production_Element);

   type Employee_Record is
      record
         Pop    : Concorde.People.Pops.Pop_Type;
         Size   : Concorde.Quantities.Quantity_Type;
         Skill  : Concorde.People.Skills.Pop_Skill;
         Wage   : Concorde.Money.Price_Type;
      end record;

   package Employee_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Employee_Record);

   type Root_Installation_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Locations.Located_Interface with
      record
         Facility         : Concorde.Facilities.Facility_Type;
         Owner            : access constant
           Concorde.Agents.Root_Agent_Type'Class;
         Manager          : access constant
           Concorde.People.Individuals.Root_Individual_Type'Class;
         Employees        : Employee_Lists.List;
         Production_Queue : Queued_Production_Lists.List;
      end record;

   overriding function Class_Name
     (Installation : Root_Installation_Type) return String
   is ("installation");

   overriding function Short_Name
     (Installation : Root_Installation_Type)
      return String
   is ("[" & Memor.To_String (Installation.Reference) & "]"
       & " " & Installation.Facility.Name);

   overriding function Identifier
     (Item : Root_Installation_Type)
      return String
   is (Concorde.Agents.Root_Agent_Type (Item).Identifier
       & "--" & Item.Facility.Identifier);

   overriding function Object_Database
     (Item : Root_Installation_Type)
      return Memor.Memor_Database;

   overriding function Delayed_Trade_Offers
     (Installation : Root_Installation_Type)
      return Boolean
   is (Installation.Is_Colony_Hub);

   overriding function Variable_Reference
     (Installation : not null access constant Root_Installation_Type)
      return access Concorde.Agents.Root_Agent_Type'Class
   is (Installation.Update.Item);

   overriding procedure Execute_Hire
     (Employer  : not null access constant Root_Installation_Type;
      Employee  : not null access constant
        Concorde.Trades.Trader_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Wage      : Concorde.Money.Price_Type);

   function Has_Manager
     (Installation : Root_Installation_Type'Class)
      return Boolean
   is (Installation.Manager /= null);

   function Manager
     (Installation : Root_Installation_Type'Class)
      return access constant
     Concorde.People.Individuals.Root_Individual_Type'Class
   is (Installation.Manager);

   package Db is
     new Memor.Database
       ("installation", Root_Installation_Type, Installation_Type);

   type Updateable_Reference
     (Item : not null access Root_Installation_Type'Class)
   is
      record
         Update : Db.Updateable_Reference (Item);
      end record;

end Concorde.Installations;
