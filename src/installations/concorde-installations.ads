private with Ada.Containers.Doubly_Linked_Lists;

private with Memor;
private with Memor.Database;

with Concorde.Money;
with Concorde.Quantities;

with Concorde.Agents;
with Concorde.Commodities;
with Concorde.Trades;

with Concorde.Facilities;

with Concorde.Locations;

limited with Concorde.Worlds;
limited with Concorde.People.Communities;
limited with Concorde.People.Individuals;

with Concorde.People.Pops;

with Concorde.Calendar;

package Concorde.Installations is

   type Root_Installation_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Facilities.Production_Interface
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

   function World
     (Installation : Root_Installation_Type'Class)
      return access constant Concorde.Worlds.Root_World_Type'Class;

   function Community
     (Installation : Root_Installation_Type'Class)
      return access constant
     Concorde.People.Communities.Root_Community_Type'Class;

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

   function Size
     (Installation : Root_Installation_Type'Class)
      return Concorde.Quantities.Quantity_Type;

   overriding procedure Execute_Production
     (Installation : in out Root_Installation_Type);

   procedure Set_Artisan_Production
     (Installation : in out Root_Installation_Type'Class;
      Facility     : Concorde.Facilities.Facility_Type)
     with Pre => Facility.Is_Artisan;

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
         Done   : Concorde.Calendar.Time;
         Size   : Concorde.Quantities.Quantity_Type;
         Output : Concorde.Commodities.Commodity_Type;
      end record;

   package Queued_Production_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Queued_Production_Element);

   type Employee_Record is
      record
         Pop            : Concorde.People.Pops.Pop_Type;
         Size           : Concorde.Quantities.Quantity_Type;
         Wage           : Concorde.Money.Price_Type;
         Contract_Days  : Positive;
         Days_Remaining : Natural;
      end record;

   package Employee_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Employee_Record);

   type Root_Installation_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Facilities.Production_Interface
     and Concorde.Locations.Located_Interface with
      record
         Facility          : Concorde.Facilities.Facility_Type;
         Size              : Concorde.Quantities.Quantity_Type;
         Owner             : access constant
           Concorde.Agents.Root_Agent_Type'Class;
         Manager           : access constant
           Concorde.People.Individuals.Root_Individual_Type'Class;
         Employees         : Employee_Lists.List;
         Production_Queue  : Queued_Production_Lists.List;
         Efficiency        : Concorde.Facilities.Process_Efficiency;
      end record;

   overriding procedure Update_Agent
     (Installation   : not null access constant Root_Installation_Type;
      Perform_Update : not null access
        procedure (Agent : in out Concorde.Agents.Root_Agent_Type'Class));

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
       & "--"
       & (if Concorde.Facilities."=" (Item.Facility, null)
          then "unassigned"
          else Item.Facility.Identifier));

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

   function Size
     (Installation : Root_Installation_Type'Class)
      return Concorde.Quantities.Quantity_Type
   is (Installation.Size);

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
