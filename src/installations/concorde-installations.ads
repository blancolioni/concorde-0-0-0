private with Memor;
private with Memor.Database;

with Concorde.Agents;
with Concorde.Trades;

with Concorde.Facilities;

with Concorde.Locations;

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

   type Installation_Type is access constant Root_Installation_Type'Class;

   type Updateable_Reference
     (Item : not null access Root_Installation_Type'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Root_Installation_Type'Class)
      return Updateable_Reference;

private

   type Root_Installation_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Locations.Located_Interface with
      record
         Facility : Concorde.Facilities.Facility_Type;
         Owner    : access constant Concorde.Agents.Root_Agent_Type'Class;
      end record;

   overriding function Class_Name
     (Installation : Root_Installation_Type) return String
   is ("installation");

   overriding function Short_Name
     (Installation : Root_Installation_Type)
      return String
   is ("[" & Memor.To_String (Installation.Reference) & "]"
       & " " & Installation.Facility.Name);

   overriding function Object_Database
     (Item : Root_Installation_Type)
      return Memor.Memor_Database;

   overriding procedure Add_Trade_Offers
     (Item   : not null access constant Root_Installation_Type);

   overriding function Delayed_Trade_Offers
     (Installation : Root_Installation_Type)
      return Boolean
   is (Installation.Is_Colony_Hub);

   overriding procedure On_Update_Start
     (Installation : in out Root_Installation_Type)
   is null;

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
