private with Memor;
private with Memor.Database;
private with Concorde.Locations;

with Concorde.Agents;
with Concorde.Calendar;
with Concorde.Facilities;
with Concorde.Trades;

limited with Concorde.Installations;

with Concorde.People.Groups;
with WL.Quantities;

package Concorde.People.Pops is

   type Pop_Size is range 1 .. 9_999_999;

   type Root_Pop_Type is
     new Concorde.Agents.Root_Agent_Type
   with private;

   function Group
     (Pop : Root_Pop_Type'Class)
      return Concorde.People.Groups.Pop_Group;

   function Size (Pop : Root_Pop_Type'Class) return Pop_Size;

   function Size_Quantity
     (Pop : Root_Pop_Type'Class)
      return WL.Quantities.Quantity_Type;

   procedure Add_Trade_Offers
     (Pop : not null access constant Root_Pop_Type);

   procedure Execute_Consumption
     (Pop : in out Root_Pop_Type'Class);

   function Has_Production
     (Pop : not null access constant Root_Pop_Type'Class)
      return Boolean;

   function Production
     (Pop : not null access constant Root_Pop_Type'Class)
      return Concorde.Facilities.Facility_Type;

   function Current_Production_Duration
     (Pop : not null access constant Root_Pop_Type'Class)
      return Duration
     with Pre => Pop.Group.Is_Artisan
     and then Pop.Has_Production;

   procedure Set_Production
     (Pop        : in out Root_Pop_Type'Class;
      Production : Concorde.Facilities.Facility_Type)
     with Pre => Pop.Group.Is_Artisan and then Production.Is_Artisan,
     Post => Concorde.Facilities."=" (Pop.Production, Production);

   type Pop_Type is access constant Root_Pop_Type'Class;

   type Updateable_Reference (Item : not null access Root_Pop_Type'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Root_Pop_Type'Class)
      return Updateable_Reference;

private

   type Consumption_Record is
      record
         Total_Needed   : WL.Quantities.Quantity_Type := WL.Quantities.Zero;
         Total_Consumed : WL.Quantities.Quantity_Type := WL.Quantities.Zero;
      end record;

   type Pop_Consumption is
     array (Concorde.People.Groups.Need_Level) of Consumption_Record;

   type Root_Pop_Type is
     new Concorde.Agents.Root_Agent_Type with
      record
         Size                : Pop_Size;
         Group               : Concorde.People.Groups.Pop_Group;
         Employer            : Concorde.Agents.Agent_Type;
         Consumption         : Pop_Consumption;
         Installation        : access constant
           Concorde.Installations.Root_Installation_Type'Class;
         Production_Started  : Concorde.Calendar.Time;
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
   is ("[" & Memor.To_String (Item.Reference) & "] "
       & Item.Group.Name);

   overriding function Variable_Reference
     (Pop : not null access constant Root_Pop_Type)
      return access Concorde.Agents.Root_Agent_Type'Class
   is (Pop.Update.Item);

   function Group
     (Pop : Root_Pop_Type'Class)
      return Concorde.People.Groups.Pop_Group
   is (Pop.Group);

   function Size (Pop : Root_Pop_Type'Class) return Pop_Size
   is (Pop.Size);

   function Size_Quantity
     (Pop : Root_Pop_Type'Class)
      return WL.Quantities.Quantity_Type
   is (WL.Quantities.To_Quantity (Float (Pop.Size)));

   function Current_Production_Duration
     (Pop : not null access constant Root_Pop_Type'Class)
      return Duration
   is (Concorde.Calendar."-"
       (Concorde.Calendar.Clock, Pop.Production_Started));

   procedure Execute_Production
     (Pop : in out Root_Pop_Type'Class)
     with Pre => Pop.Group.Is_Artisan
     and then Pop.Has_Production;

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
