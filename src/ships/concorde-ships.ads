private with Ada.Containers.Vectors;
private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

private with Memor.Database;

with Memor;

limited with Concorde.Factions;
limited with Concorde.Systems;
limited with Concorde.Worlds;

with Concorde.Commodities;

with Concorde.Agents;
with Concorde.Components;
with Concorde.Locations;
with Concorde.Modules;
with Concorde.Objects;
with Concorde.Trades;

with Concorde.Quantities;

private with Newton;
with Concorde.Calendar;

with Concorde.Events;

package Concorde.Ships is

   type Root_Ship_Type is
     new Concorde.Agents.Root_Agent_Type
     and Memor.Identifier_Record_Type
     and Concorde.Objects.User_Named_Object_Interface
   with private;

   overriding procedure Set_Location
     (Ship     : in out Root_Ship_Type;
      Location : Concorde.Locations.Object_Location);

   function Long_Name (Ship : Root_Ship_Type'Class) return String;

   function Short_Description (Ship : Root_Ship_Type'Class) return String;

   function Alive
     (Ship : Root_Ship_Type'Class)
      return Boolean;

   function Owner
     (Ship : Root_Ship_Type'Class)
      return access constant Concorde.Factions.Root_Faction_Type'Class;

   procedure Set_Owner
     (Ship   : in out Root_Ship_Type'Class;
      New_Owner : not null access constant
        Concorde.Factions.Root_Faction_Type'Class);

   function Destination
     (Ship : Root_Ship_Type'Class)
      return Concorde.Locations.Object_Location;

   procedure Set_Destination
     (Ship         : in out Root_Ship_Type'Class;
      World        : not null access constant
        Concorde.Worlds.Root_World_Type'Class;
      Start_Time   : Concorde.Calendar.Time;
      Journey_Time : Duration);

   procedure Set_Destination
     (Ship         : in out Root_Ship_Type'Class;
      Destination  : Concorde.Locations.Object_Location;
      Start_Time   : Concorde.Calendar.Time;
      Journey_Time : Duration);

   procedure Set_Jump_Destination
     (Ship         : in out Root_Ship_Type'Class;
      System       : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Start_Time   : Concorde.Calendar.Time;
      Journey_Time : Duration);

   procedure Clear_Destination
     (Ship   : in out Root_Ship_Type'Class);

   function Damage
     (Ship : Root_Ship_Type'Class)
      return Unit_Real
     with Inline;

   function Shields
     (Ship : Root_Ship_Type'Class)
      return Unit_Real
     with Inline;

   procedure Hit
     (Target : in out Root_Ship_Type'Class;
      Damage : Natural);

   procedure Repair
     (Ship   : in out Root_Ship_Type'Class;
      Points : Positive);

   function Maximum_Thrust
     (Ship : Root_Ship_Type'Class)
      return Non_Negative_Real;
   --  Newtons

   function Turn
     (Ship : Root_Ship_Type'Class)
      return Non_Negative_Real
   is (1.0);
   --  degrees/s

   function Size
     (Ship : Root_Ship_Type'Class)
      return Size_Type;

   procedure Update_Power
     (Ship : Root_Ship_Type'Class);

   procedure Update_Damage
     (Ship : Root_Ship_Type'Class);

   type Module_Position is
      record
         X, Y, Z : Integer;
      end record;

   type Orientation_Axis is (X_Axis, Y_Axis, Z_Axis);

   type Module_Orientation is
      record
         Axis    : Orientation_Axis;
         Forward : Boolean;
      end record;

   type Mounted_Module is private;

   function Get_Module
     (Ship  : Root_Ship_Type'Class;
      Mount : Mounted_Module)
      return Concorde.Modules.Module_Type;

   function Get_Orientation
     (Ship  : Root_Ship_Type'Class;
      Mount : Mounted_Module)
      return Module_Orientation;

   function Get_Position
     (Ship  : Root_Ship_Type'Class;
      Mount : Mounted_Module)
      return Module_Position;

   function Has_Effective_Weapon
     (Ship : Root_Ship_Type'Class)
      return Boolean;

   function Has_Effective_Engine
     (Ship : Root_Ship_Type'Class)
      return Boolean;

   type Array_Of_Mounted_Modules is
     array (Positive range <>) of Mounted_Module;

   function Get_Class_Mounts
     (Ship  : Root_Ship_Type'Class;
      Class : Concorde.Components.Component_Class)
      return Array_Of_Mounted_Modules;

   function Get_Matching_Mounts
     (Ship  : Root_Ship_Type'Class;
      Match : not null access
        function (Module : Concorde.Modules.Module_Type)
      return Boolean)
      return Array_Of_Mounted_Modules;

   function Get_Drive_Mounts
     (Ship : Root_Ship_Type'Class)
      return Array_Of_Mounted_Modules
   is (Ship.Get_Class_Mounts (Concorde.Components.Drive));

   function Get_Weapon_Mounts
     (Ship : Root_Ship_Type'Class)
      return Array_Of_Mounted_Modules;

   function Get_Damaged_Mounts
     (Ship : Root_Ship_Type'Class)
      return Array_Of_Mounted_Modules;

   function Get_Effective_Mounts
     (Ship : Root_Ship_Type'Class)
      return Array_Of_Mounted_Modules;

   function Empty_Mass
     (Ship : Root_Ship_Type'Class)
      return Non_Negative_Real;

   function Current_Mass
     (Ship : Root_Ship_Type'Class)
      return Non_Negative_Real;

   function Standard_Full_Mass
     (Ship : Root_Ship_Type'Class)
      return Non_Negative_Real;

   function Tank_Size
     (Ship : Root_Ship_Type'Class)
      return Non_Negative_Real;

   function Hold_Size
     (Ship : Root_Ship_Type'Class)
      return Non_Negative_Real;

   function Hold_Quantity
     (Ship : Root_Ship_Type'Class)
      return Concorde.Quantities.Quantity_Type
   is (Ship.Maximum_Quantity);

   type Ship_Type is access constant Root_Ship_Type'Class;

   type Root_Ship_Event is
     new Concorde.Events.Root_Event_Type with private;

   function Ship_Event
     (Time_Stamp : Concorde.Calendar.Time;
      Ship       : not null access constant Root_Ship_Type'Class)
      return Root_Ship_Event'Class;

   function Ship (Event : Root_Ship_Event'Class) return Ship_Type;

   function Count_Ships
     (Test : not null access function
        (Ship : Ship_Type)
      return Boolean)
      return Natural;

   type Updateable_Reference (Item : not null access Root_Ship_Type'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Root_Ship_Type'Class)
      return Updateable_Reference;

private

   type Mounted_Module is new Positive;

   type Module_Layout_Record is
      record
         Module       : Concorde.Modules.Module_Type;
         Left_Low_Aft : Module_Position;
         Orientation  : Module_Orientation;
      end record;

   package Module_Vectors is
     new Ada.Containers.Vectors
       (Positive, Module_Layout_Record);

   type Ship_Order_Type is (No_Order, Trade, Buy, Sell, Colonise);

   type Ship_Order_Record is
      record
         Order      : Ship_Order_Type;
         World      : access constant Concorde.Worlds.Root_World_Type'Class;
         Next       : access constant Concorde.Worlds.Root_World_Type'Class;
         Commodity  : Concorde.Commodities.Commodity_Type;
         Quantity   : Concorde.Quantities.Quantity_Type;
      end record;

   package List_Of_Orders is
     new Ada.Containers.Doubly_Linked_Lists (Ship_Order_Record);

   type Root_Ship_Type is
     new Concorde.Agents.Root_Agent_Type
     and Memor.Identifier_Record_Type
     and Concorde.Locations.Located_Interface
     and Concorde.Objects.User_Named_Object_Interface with
      record
         Identity              : String (1 .. 6);
         Ship_Name             : Ada.Strings.Unbounded.Unbounded_String;
         Owner                 : access constant
           Concorde.Factions.Root_Faction_Type'Class;
         Destination           : Concorde.Locations.Object_Location;
         Start_Time            : Concorde.Calendar.Time;
         Arrival_Time          : Concorde.Calendar.Time;
         Moving                : Boolean := False;
         Jumping               : Boolean := False;
         Cycle_Orders          : Boolean := False;
         Alive                 : Boolean := True;
         Is_Trader             : Boolean := False;
         Have_Trade_Orders     : Boolean := False;
         Orders                : List_Of_Orders.List;
         Buy_Requirements      : Concorde.Commodities.Root_Stock_Type;
         Trade_From            : access constant Worlds.Root_World_Type'Class;
         Trade_To              : access constant Worlds.Root_World_Type'Class;
         Structure             : Module_Vectors.Vector;
         Size                  : Size_Type;
         Current_Damage        : Unit_Real := 0.0;
         Current_Shields       : Unit_Real := 0.0;
         Position              : Newton.Vector_3;
         Velocity              : Newton.Vector_3;
         Orientation           : Newton.Matrix_3;
      end record;

   package Ship_Vectors is
     new Ada.Containers.Vectors (Positive, Ship_Type);

   overriding function Class_Name (Ship : Root_Ship_Type) return String
   is ("ship");

   overriding function Object_Database
     (Ship : Root_Ship_Type)
      return Memor.Memor_Database;

   overriding function Name
     (Ship : Root_Ship_Type)
      return String
   is (Ada.Strings.Unbounded.To_String (Ship.Ship_Name));

   overriding procedure Set_Name
     (Ship : in out Root_Ship_Type;
      Name : String);

   overriding function Short_Name
     (Ship : Root_Ship_Type)
      return String
   is (Ship.Identity & " " & Ship.Name);

   overriding function Identifier
     (Ship : Root_Ship_Type)
      return String
   is (Ship.Identity);

   overriding function Market_Resident
     (Ship : Root_Ship_Type)
      return Boolean
   is (False);

--     overriding procedure Add_Trade_Offers
--       (Ship : not null access constant Root_Ship_Type)
--     is null;

   overriding function Location_At
     (Ship : Root_Ship_Type;
      Time : Concorde.Calendar.Time)
      return Concorde.Locations.Object_Location;

   overriding function Offer_Strategy
     (Ship : Root_Ship_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Trades.Offer_Price_Strategy
   is (Concorde.Trades.Average_Price);

   overriding function Variable_Reference
     (Ship : not null access constant Root_Ship_Type)
      return access Concorde.Agents.Root_Agent_Type'Class
   is (Ship.Update.Item);

   function Destination
     (Ship : Root_Ship_Type'Class)
      return Concorde.Locations.Object_Location
   is (Ship.Destination);

   type Root_Ship_Event is
     new Concorde.Events.Root_Event_Type with
      record
         Ship : Ship_Type;
      end record;

   function Ship (Event : Root_Ship_Event'Class) return Ship_Type
   is (Event.Ship);

   package Db is
     new Memor.Database
       ("ship", Root_Ship_Type, Ship_Type);

   type Updateable_Reference
     (Item : not null access Root_Ship_Type'Class)
   is
      record
         Update : Db.Updateable_Reference (Item);
      end record;

end Concorde.Ships;
