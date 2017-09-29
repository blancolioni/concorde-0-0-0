private with Memor;
private with Memor.Database;
private with Concorde.People.Skills.Lists;
private with Concorde.Locations;

with Concorde.Agents;
with Concorde.Trades;

with Concorde.People.Groups;
with Concorde.Quantities;

package Concorde.People.Pops is

   type Pop_Size is range 1 .. 9_999_999;

   type Root_Pop_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.People.Groups.Affiliation_Interface
   with private;

   function Size (Pop : Root_Pop_Type'Class) return Pop_Size;

   function Size_Quantity
     (Pop : Root_Pop_Type'Class)
      return Concorde.Quantities.Quantity_Type;

   overriding function Affiliation
     (Pop   : Root_Pop_Type;
      Group : Concorde.People.Groups.Pop_Group)
      return Concorde.People.Groups.Affiliation_Range;

   type Pop_Type is access constant Root_Pop_Type'Class;

   type Updateable_Reference (Item : not null access Root_Pop_Type'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Root_Pop_Type'Class)
      return Updateable_Reference;

private

   type Root_Pop_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.People.Groups.Affiliation_Interface with
      record
         Size   : Pop_Size;
         Groups : Concorde.People.Groups.Affiliation_Vector;
         Skills : Concorde.People.Skills.Lists.List;
      end record;

   overriding function Class_Name
     (Pop : Root_Pop_Type) return String
   is ("pop");

   overriding function Identifier
     (Pop : Root_Pop_Type) return String
   is (Concorde.Agents.Root_Agent_Type (Pop).Identifier
       & "--" & Concorde.Locations.Short_Name (Pop.Current_Location)
       & Integer'Image (-1 * Integer (Pop.Size)));

   overriding function Object_Database
     (Item : Root_Pop_Type)
      return Memor.Memor_Database;

   overriding function Short_Name
     (Item : Root_Pop_Type)
      return String
   is ("[" & Memor.To_String (Item.Reference) & "]"
       & (if Item.Skills.Is_Empty
          then ""
          else " " & Item.Skills.First_Element.Name)
       & " "
       & Item.Wealth_Group.Name);

   overriding procedure Add_Trade_Offers
     (Item   : not null access constant Root_Pop_Type);

   overriding procedure Before_Market
     (Pop : in out Root_Pop_Type);

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
