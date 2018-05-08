private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;
private with Memor;
private with Memor.Database;

with Xi.Color;

limited with Concorde.Factions;

with Concorde.Geometry;
with WL.Quantities;

with Concorde.Calendar;
with Concorde.Locations;
with Concorde.Objects;
with Concorde.Ships;

with Concorde.Ships.Lists;

with Concorde.Commodities;
with Concorde.Government;

with Concorde.Events;
with Concorde.Signals;

with Concorde.Vectors;

package Concorde.Systems is

   Signal_Ship_Departed : constant Concorde.Signals.Signal_Type :=
                            "signal-ship-departed";
   Signal_Ship_Arrived  : constant Concorde.Signals.Signal_Type :=
                            "signal-ship-arrived";

   type Star_System_Object_Interface is limited interface
     and Concorde.Objects.Massive_Object_Interface
     and Concorde.Objects.Named_Object_Interface
     and Concorde.Locations.Located_Interface;

   function Radius (Object : Star_System_Object_Interface)
                    return Non_Negative_Real
                    is abstract;

   function Primary (Object : Star_System_Object_Interface)
                     return access Star_System_Object_Interface'Class
                     is abstract;

   function Semimajor_Axis (Object : Star_System_Object_Interface)
                            return Non_Negative_Real
                            is abstract;

   function Age (Object : Star_System_Object_Interface)
                 return Non_Negative_Real
                 is abstract;

   function Orbit_Progress
     (Object : Star_System_Object_Interface)
      return Concorde.Geometry.Radians
      is abstract;

   function Eccentricity (Object : Star_System_Object_Interface)
                          return Unit_Real
                          is abstract;

   function Period (Object : Star_System_Object_Interface'Class)
                    return Non_Negative_Real;

--     function Primary_Relative_Position
--       (Object : Star_System_Object_Interface'Class)
--        return Concorde.Vectors.Vector_3;

   type Root_Star_System_Type is
     new Concorde.Objects.Root_User_Named_Object_Type
   with private;

   function System
     (Object : Star_System_Object_Interface)
      return access constant Root_Star_System_Type'Class
      is abstract;

   function Index (System : Root_Star_System_Type'Class) return Positive;
   function X (System : Root_Star_System_Type'Class) return Real;
   function Y (System : Root_Star_System_Type'Class) return Real;
   function Z (System : Root_Star_System_Type'Class) return Real;

   procedure Add_Object
     (System   : in out Root_Star_System_Type'Class;
      Object   : not null access Star_System_Object_Interface'Class);

   type Main_Star_System_Object_Interface is limited interface
     and Star_System_Object_Interface;

   function Color (Object : Main_Star_System_Object_Interface)
                   return Xi.Color.Xi_Color
                   is abstract;

   function Main_Object
     (System : Root_Star_System_Type'Class)
      return access Main_Star_System_Object_Interface'Class;

   function Jump_Departure_Point
     (From : Root_Star_System_Type'Class;
      To   : not null access Root_Star_System_Type'Class)
      return Concorde.Vectors.Vector_3;

   function Jump_Arrival_Point
     (Arrival_System   : Root_Star_System_Type'Class;
      Departure_System : not null access Root_Star_System_Type'Class)
      return Concorde.Vectors.Vector_3;

   procedure Scan_System_Objects
     (System : Root_Star_System_Type'Class;
      Process : not null access
        procedure (System_Object : Star_System_Object_Interface'Class));

   procedure Scan_System_Objects
     (System  : Root_Star_System_Type'Class;
      Process : not null access
        procedure
          (System_Object : not null access constant
               Star_System_Object_Interface'Class));

   function Owner
     (System : Root_Star_System_Type'Class)
      return access constant Concorde.Factions.Root_Faction_Type'Class;

   function Owned_By
     (System : Root_Star_System_Type'Class;
      Faction : Concorde.Factions.Root_Faction_Type'Class)
      return Boolean;

   function Loyalty
     (System : Root_Star_System_Type'Class)
      return Unit_Real;

   function Owned
     (System : Root_Star_System_Type'Class)
      return Boolean
   is (System.Owner /= null);

   function Capital (System : Root_Star_System_Type'Class)
                     return Boolean;

   function Ships
     (System : Root_Star_System_Type'Class)
      return Natural;

   procedure Add_Ship
     (System : in out Root_Star_System_Type'Class;
      Ship   : not null access constant
        Concorde.Ships.Root_Ship_Type'Class);

   procedure Remove_Ship
     (System : in out Root_Star_System_Type'Class;
      Ship   : not null access constant
        Concorde.Ships.Root_Ship_Type'Class);

   procedure Remove_Dead_Ships
     (System : in out Root_Star_System_Type'Class);

   procedure Arriving
     (System : in out Root_Star_System_Type'Class;
      Ship   : not null access constant
        Concorde.Ships.Root_Ship_Type'Class;
      Time   : Concorde.Calendar.Time);

   procedure Departing
     (System : in out Root_Star_System_Type'Class;
      Ship   : not null access constant
        Concorde.Ships.Root_Ship_Type'Class;
      Time   : Concorde.Calendar.Time);

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
                         return Concorde.Calendar.Time;

   function Last_Battle_Size
     (System : Root_Star_System_Type'Class)
      return Natural;

   procedure Set_Owner
     (System : in out Root_Star_System_Type'Class;
      New_Owner : not null access constant
        Concorde.Factions.Root_Faction_Type'Class);

   procedure Set_Capital
     (System     : in out Root_Star_System_Type'Class;
      Is_Capital : Boolean);

   type System_Influence_Boundary is
     array (Positive range <>) of Point_Type;

   function Influence_Boundary
     (System : Root_Star_System_Type'Class)
      return System_Influence_Boundary;

   type Star_System_Type is access constant Root_Star_System_Type'Class;

   function System_Count return Natural;
   function Get (Index : Positive) return Star_System_Type
     with Pre => Index <= System_Count;

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

   procedure Scan_Systems
     (Test : not null access
        function (System : Star_System_Type) return Boolean;
      Process : not null access
        procedure (System : Star_System_Type));

   procedure Scan_Systems
     (Process : not null access
        procedure (System : Star_System_Type));

   procedure Update_Systems
     (Test    : not null access
        function (System : Root_Star_System_Type'Class) return Boolean;
      Update  : not null access
        procedure (System : in out Root_Star_System_Type'Class));

   procedure Update_Systems
     (Update : not null access
        procedure (System : in out Root_Star_System_Type'Class));

   type Updateable_Reference
     (Item : not null access Root_Star_System_Type'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Root_Star_System_Type'Class)
      return Updateable_Reference;

   function Create_Ship_Event
     (Time_Stamp : Concorde.Calendar.Time;
      Ship   : not null access constant
        Concorde.Ships.Root_Ship_Type'Class)
      return Concorde.Events.Root_Event_Type'Class;

private

   type Ship_Event is
     new Concorde.Events.Root_Event_Type with
      record
         Ship : Concorde.Ships.Ship_Type;
      end record;

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
         Size          : WL.Quantities.Quantity_Type;
         Original_Size : WL.Quantities.Quantity_Type;
      end record;

   type System_Object_Record is
      record
         Object  : access Star_System_Object_Interface'Class;
      end record;

   package System_Object_Lists is
     new Ada.Containers.Doubly_Linked_Lists (System_Object_Record);

   type Root_Star_System_Type is
     new Concorde.Objects.Root_User_Named_Object_Type with
      record
         Index          : Positive;
         X, Y, Z        : Real;
         Production     : Non_Negative_Real;
         Capacity       : Non_Negative_Real;
         Progress       : Non_Negative_Real := 0.0;
         Main_Object    : access Main_Star_System_Object_Interface'Class;
         Objects        : System_Object_Lists.List;
         Ships          : Concorde.Ships.Lists.List;
         Capital        : Boolean := False;
         Last_Battle    : Concorde.Calendar.Time;
         Battle_Size    : Natural := 0;
         Last_Attacker  : Star_System_Type := null;
         Owner          : access constant
           Concorde.Factions.Root_Faction_Type'Class;
         Original_Owner : access constant
           Concorde.Factions.Root_Faction_Type'Class;
         Government     : Concorde.Government.Government_Type;
         Loyalty        : Unit_Real := 1.0;
         Edges          : Edge_Info_Lists.List;
         Boundary       : access System_Influence_Boundary;
      end record;

   overriding function Object_Database
     (Star_System : Root_Star_System_Type)
      return Memor.Memor_Database;

   overriding procedure Load
     (Star_System : in out Root_Star_System_Type);

   package System_Vectors is
     new Ada.Containers.Vectors (Positive, Star_System_Type);

   System_Vector : System_Vectors.Vector;

   function System_Count return Natural
   is (System_Vector.Last_Index);

   function Get (Index : Positive) return Star_System_Type
   is (System_Vector.Element (Index));

   package Db is
     new Memor.Database
       ("system", Root_Star_System_Type, Star_System_Type);

   type Updateable_Reference
     (Item : not null access Root_Star_System_Type'Class)
   is
      record
         Update : Db.Updateable_Reference (Item);
      end record;

end Concorde.Systems;
