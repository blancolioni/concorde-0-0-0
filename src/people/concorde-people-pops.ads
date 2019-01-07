private with Memor;
private with Memor.Database;
private with Concorde.Locations;

with Concorde.Money;
with Concorde.Quantities;

with Concorde.Network;

with Concorde.Agents;
with Concorde.Commodities;
with Concorde.Facilities;
with Concorde.Trades;

with Concorde.People.Groups;

package Concorde.People.Pops is

   type Pop_Size is range 1 .. 9_999_999;

   type Root_Pop_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Network.Expression_Object_Interface
   with private;

   type Pop_Type is access constant Root_Pop_Type'Class;

   function Group
     (Pop : Root_Pop_Type'Class)
      return Concorde.People.Groups.Pop_Group;

   function Size (Pop : Root_Pop_Type'Class) return Pop_Size;

   function Size_Quantity
     (Pop : Root_Pop_Type'Class)
      return Concorde.Quantities.Quantity_Type;

   type Updateable_Reference (Item : not null access Root_Pop_Type'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Root_Pop_Type'Class)
      return Updateable_Reference;

private

   type Root_Pop_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Network.Expression_Object_Interface with
      record
         Size         : Pop_Size;
         Group        : Concorde.People.Groups.Pop_Group;
         Base_Income  : Concorde.Network.Node_State_Access;
         Apathy       : Unit_Real := 0.0;
      end record;

   overriding procedure Update_Agent
     (Pop            : not null access constant Root_Pop_Type;
      Perform_Update : not null access
        procedure (Agent : in out Concorde.Agents.Root_Agent_Type'Class));

   overriding function Daily_Budget
     (Pop       : Root_Pop_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Money_Type;

   overriding function Daily_Needs
     (Pop       : Root_Pop_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type;

   overriding function Daily_Desire
     (Pop       : Root_Pop_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type;

   overriding function Daily_Supply
     (Pop       : Root_Pop_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type;

   overriding function Class_Name
     (Pop : Root_Pop_Type) return String
   is ("pop");

   overriding function Identifier
     (Pop : Root_Pop_Type) return String
   is (Concorde.Agents.Root_Agent_Type (Pop).Identifier
       & "--" & Pop.Group.Identifier);

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

   overriding function Get_Value
     (Pop : Root_Pop_Type)
      return Concorde.Network.Expression_Value
   is (Concorde.Network.To_Expression_Value (Real (Pop.Size)));

   overriding function Has_Field
     (Pop   : Root_Pop_Type;
      Name  : String)
      return Boolean;

   overriding function Get_Field_Value
     (Pop   : Root_Pop_Type;
      Name  : String)
      return Concorde.Network.Expression_Value;

   function Group
     (Pop : Root_Pop_Type'Class)
      return Concorde.People.Groups.Pop_Group
   is (Pop.Group);

   function Size (Pop : Root_Pop_Type'Class) return Pop_Size
   is (Pop.Size);

   function Size_Quantity
     (Pop : Root_Pop_Type'Class)
      return Concorde.Quantities.Quantity_Type
   is (Concorde.Quantities.To_Quantity (Non_Negative_Real (Pop.Size)));

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
