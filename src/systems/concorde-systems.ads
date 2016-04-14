private with Ada.Containers.Doubly_Linked_Lists;
private with Memor;

limited with Concorde.Empires;

with Concorde.Quantities;

with Concorde.Dates;
with Concorde.Objects;
with Concorde.Ships;

with Concorde.Ships.Lists;

with Concorde.Installations;
with Concorde.People.Pops;

with Concorde.Commodities;
with Concorde.Markets;

private with Concorde.People.Pops.Lists;
private with Concorde.Installations.Lists;

package Concorde.Systems is

   type Root_Star_System_Type is
     new Concorde.Objects.Root_Named_Object_Type with private;

   function Index (System : Root_Star_System_Type'Class) return Positive;
   function X (System : Root_Star_System_Type'Class) return Real;
   function Y (System : Root_Star_System_Type'Class) return Real;

   function Owner
     (System : Root_Star_System_Type'Class)
      return access constant Concorde.Empires.Root_Empire_Type'Class;

   function Owned_By
     (System : Root_Star_System_Type'Class;
      Empire : Concorde.Empires.Root_Empire_Type'Class)
      return Boolean;

   function Resource
     (System : Root_Star_System_Type'Class)
      return Concorde.Commodities.Commodity_Type;

   function Resource_Accessibility
     (System : Root_Star_System_Type'Class)
      return Unit_Real;

   function Resource_Concentration
     (System : Root_Star_System_Type'Class)
      return Unit_Real;

   function Resource_Size
     (System : Root_Star_System_Type'Class)
      return Concorde.Quantities.Quantity;

   function Loyalty
     (System : Root_Star_System_Type'Class)
      return Unit_Real;

   function Owned
     (System : Root_Star_System_Type'Class)
      return Boolean
   is (System.Owner /= null);

   function Production (System : Root_Star_System_Type'Class)
                        return Non_Negative_Real;

   function Capacity (System : Root_Star_System_Type'Class)
                      return Non_Negative_Real;

   function Capital (System : Root_Star_System_Type'Class)
                     return Boolean;

   procedure Add_Pop
     (System : in out Root_Star_System_Type'Class;
      Pop    : Concorde.People.Pops.Pop_Type);

   procedure Add_Installation
     (System       : in out Root_Star_System_Type'Class;
      Installation : Concorde.Installations.Installation_Type);

   function Ships
     (System : Root_Star_System_Type'Class)
      return Natural;

   procedure Add_Ship
     (System : in out Root_Star_System_Type'Class;
      Ship   : Concorde.Ships.Ship_Type);

   procedure Remove_Ship
     (System : in out Root_Star_System_Type'Class;
      Ship   : Concorde.Ships.Ship_Type);

   procedure Remove_Dead_Ships
     (System : in out Root_Star_System_Type'Class);

   procedure Arriving
     (System : in out Root_Star_System_Type'Class;
      Ship   : Concorde.Ships.Ship_Type);

   procedure Departing
     (System : in out Root_Star_System_Type'Class;
      Ship   : Concorde.Ships.Ship_Type);

   procedure Commit_Ship_Movement
     (System : not null access Root_Star_System_Type'Class);

   procedure Clear_Ship_Movement
     (System : in out Root_Star_System_Type'Class);

   procedure Add_Traffic
     (From  : in out Root_Star_System_Type'Class;
      To    : not null access constant Root_Star_System_Type'Class;
      Count : Positive := 1);

   function Traffic
     (From : Root_Star_System_Type'Class;
      To   : not null access constant Root_Star_System_Type'Class)
      return Natural;

   procedure Get_Ships
     (System    : Root_Star_System_Type'Class;
      Result    : in out Concorde.Ships.Lists.List);

   function Last_Battle (System : Root_Star_System_Type'Class)
                         return Concorde.Dates.Date_Type;

   function Last_Battle_Size
     (System : Root_Star_System_Type'Class)
      return Natural;

   procedure Set_Owner
     (System : in out Root_Star_System_Type'Class;
      New_Owner : not null access constant
        Concorde.Empires.Root_Empire_Type'Class);

   procedure Set_Capital
     (System     : in out Root_Star_System_Type'Class;
      Is_Capital : Boolean);

   procedure Set_Production
     (System : in out Root_Star_System_Type'Class;
      New_Production : Non_Negative_Real);

   procedure Set_Capacity
     (System : in out Root_Star_System_Type'Class;
      New_Capacity : Non_Negative_Real);

   type System_Influence_Boundary is
     array (Positive range <>) of Point_Type;

   function Influence_Boundary
     (System : Root_Star_System_Type'Class)
      return System_Influence_Boundary;

   type Star_System_Type is access constant Root_Star_System_Type'Class;

   procedure Battle
     (System : in out Root_Star_System_Type'Class;
      Size   : Positive);

   function Distance
     (System_1, System_2 : Star_System_Type)
      return Non_Negative_Real;

   function Get_Index
     (System : Star_System_Type)
      return Positive
   is (System.Index);

private

   type Edge_Info is
      record
         To      : Star_System_Type;
         Traffic : Natural := 0;
      end record;

   package Edge_Info_Lists is
      new Ada.Containers.Doubly_Linked_Lists (Edge_Info);

   type Deposit_Record is
      record
         Resource      : Concorde.Commodities.Commodity_Type;
         Accessibility : Unit_Real;
         Concentration : Unit_Real;
         Size          : Concorde.Quantities.Quantity;
      end record;

   type Root_Star_System_Type is
     new Concorde.Objects.Root_Named_Object_Type with
      record
         Index          : Positive;
         X, Y           : Real;
         Production     : Non_Negative_Real;
         Capacity       : Non_Negative_Real;
         Progress       : Non_Negative_Real := 0.0;
         Ships          : Concorde.Ships.Lists.List;
         Arriving       : Concorde.Ships.Lists.List;
         Departing      : Concorde.Ships.Lists.List;
         Capital        : Boolean := False;
         Last_Battle    : Concorde.Dates.Date_Type := 0;
         Battle_Size    : Natural := 0;
         Last_Attacker  : Star_System_Type := null;
         Owner          : access constant
           Concorde.Empires.Root_Empire_Type'Class;
         Original_Owner : access constant
           Concorde.Empires.Root_Empire_Type'Class;
         Loyalty        : Unit_Real := 1.0;
         Edges          : Edge_Info_Lists.List;
         Boundary       : access System_Influence_Boundary;
         Pops           : Concorde.People.Pops.Lists.List;
         Installations  : Concorde.Installations.Lists.List;
         Deposit        : Deposit_Record;
         Market         : Concorde.Markets.Market_Type;
      end record;

   overriding function Object_Database
     (Star_System : Root_Star_System_Type)
      return Memor.Root_Database_Type'Class;
end Concorde.Systems;
