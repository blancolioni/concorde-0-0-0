private with Ada.Containers.Doubly_Linked_Lists;

private with Memor.Database;

with WL.Quantities;

with Memor;

with Concorde.Commodities;
with Concorde.Objects;

package Concorde.People.Groups is

   type Need_Level is (Basic, Daily, Desire);

   type Wealth_Level is (Poor, Middle, Rich);

   type Root_Pop_Group is
     new Concorde.Objects.Root_Localised_Object_Type with private;

   function Initial_Cash_Factor
     (Group : Root_Pop_Group'Class)
      return Non_Negative_Real;

   function Unemployment
     (Group : Root_Pop_Group'Class)
      return Boolean;

   function Is_Artisan
     (Group : Root_Pop_Group'Class)
      return Boolean;

   function Is_Soldier
     (Group : Root_Pop_Group'Class)
      return Boolean;

   function Is_Spacer
     (Group : Root_Pop_Group'Class)
      return Boolean;

   function Is_Slave
     (Group : Root_Pop_Group'Class)
      return Boolean;

   function Work_Commodity
     (Group : Root_Pop_Group'Class)
      return Concorde.Commodities.Commodity_Type
     with Pre => Group.Unemployment;

   procedure Scan_Needs
     (Group   : Root_Pop_Group'Class;
      Level   : Need_Level;
      Size    : WL.Quantities.Quantity_Type;
      Process : not null access
        procedure (Commodity : Concorde.Commodities.Commodity_Type;
                   Quantity : WL.Quantities.Quantity_Type));

   type Pop_Group is access constant Root_Pop_Group'Class;

   function Get (Name : String) return Pop_Group;

private

   type Need_Record is
      record
         Commodity     : Concorde.Commodities.Commodity_Type;
         Need_Quantity : WL.Quantities.Quantity_Type;
         Pop_Quantity  : WL.Quantities.Quantity_Type;
      end record;
   --  'Pop_Quantity' people need 'Need_Quantity' commodities

   package Needs_List is
     new Ada.Containers.Doubly_Linked_Lists (Need_Record);

   type Needs_Array is array (Need_Level) of Needs_List.List;

   type Root_Pop_Group is
     new Concorde.Objects.Root_Localised_Object_Type with
      record
         Unemployment        : Boolean;
         Is_Artisan          : Boolean;
         Is_Soldier          : Boolean;
         Is_Spacer           : Boolean;
         Is_Slave            : Boolean;
         Wealth              : Wealth_Level;
         Initial_Cash_Factor : Non_Negative_Real;
         Max_Size            : WL.Quantities.Quantity_Type;
         Work_Commodity      : Concorde.Commodities.Commodity_Type;
         Needs               : Needs_Array;
      end record;

   overriding function Object_Database
     (Item : Root_Pop_Group)
      return Memor.Memor_Database;

   function Initial_Cash_Factor
     (Group : Root_Pop_Group'Class)
      return Non_Negative_Real
   is (Group.Initial_Cash_Factor);

   function Unemployment
     (Group : Root_Pop_Group'Class)
      return Boolean
   is (Group.Unemployment);

   function Is_Artisan
     (Group : Root_Pop_Group'Class)
      return Boolean
   is (Group.Is_Artisan);

   function Is_Soldier
     (Group : Root_Pop_Group'Class)
      return Boolean
   is (Group.Is_Soldier);

   function Is_Spacer
     (Group : Root_Pop_Group'Class)
      return Boolean
   is (Group.Is_Spacer);

   function Is_Slave
     (Group : Root_Pop_Group'Class)
      return Boolean
   is (Group.Is_Slave);

   function Work_Commodity
     (Group : Root_Pop_Group'Class)
      return Concorde.Commodities.Commodity_Type
   is (Group.Work_Commodity);

   package Db is
     new Memor.Database
       ("pop-group", Root_Pop_Group, Pop_Group);

   overriding function Object_Database
     (Item : Root_Pop_Group)
      return Memor.Memor_Database
   is (Db.Get_Database);

end Concorde.People.Groups;
