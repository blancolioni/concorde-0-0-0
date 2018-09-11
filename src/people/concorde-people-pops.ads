private with Ada.Containers.Doubly_Linked_Lists;

private with Memor;
private with Memor.Database;
private with Concorde.Locations;

with Concorde.Agents;
with Concorde.Facilities;
with Concorde.Trades;

with Concorde.People.Groups;
with WL.Quantities;

package Concorde.People.Pops is

   type Pop_Size is range 1 .. 9_999_999;

   type Root_Pop_Type is
     new Concorde.Agents.Root_Agent_Type
   with private;

   function Size (Pop : Root_Pop_Type'Class) return Pop_Size;

   function Size_Quantity
     (Pop : Root_Pop_Type'Class)
      return WL.Quantities.Quantity_Type;

   type Pop_Type is access constant Root_Pop_Type'Class;

   type Updateable_Reference (Item : not null access Root_Pop_Type'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Root_Pop_Type'Class)
      return Updateable_Reference;

private

   type Group_Membership_Record is
      record
         Group    : Concorde.People.Groups.Pop_Group;
         Strength : Unit_Real := 1.0;
      end record;

   package Group_Membership_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Group_Membership_Record);

   type Root_Pop_Type is
     new Concorde.Agents.Root_Agent_Type with
      record
         Size   : Pop_Size;
         Groups : Group_Membership_Lists.List;
         Apathy : Unit_Real := 0.0;
      end record;

   overriding function Class_Name
     (Pop : Root_Pop_Type) return String
   is ("pop");

   overriding function Identifier
     (Pop : Root_Pop_Type) return String
   is (Concorde.Agents.Root_Agent_Type (Pop).Identifier
       & "--" & Concorde.Locations.Short_Name (Pop.Current_Location)
       & "--"
       & WL.Quantities.Show (Pop.Size_Quantity));

   overriding function Object_Database
     (Item : Root_Pop_Type)
      return Memor.Memor_Database;

   overriding function Short_Name
     (Item : Root_Pop_Type)
      return String
   is ("[" & Memor.To_String (Item.Reference) & "] ");

   overriding function Variable_Reference
     (Pop : not null access constant Root_Pop_Type)
      return access Concorde.Agents.Root_Agent_Type'Class
   is (Pop.Update.Item);

   function Size (Pop : Root_Pop_Type'Class) return Pop_Size
   is (Pop.Size);

   function Size_Quantity
     (Pop : Root_Pop_Type'Class)
      return WL.Quantities.Quantity_Type
   is (WL.Quantities.To_Quantity (Float (Pop.Size)));

   package Db is
     new Memor.Database
       ("pop", Root_Pop_Type, Pop_Type);

   type Updateable_Reference
     (Item : not null access Root_Pop_Type'Class)
   is
      record
         Update : Db.Updateable_Reference (Item);
      end record;

end Concorde.People.Pops;
