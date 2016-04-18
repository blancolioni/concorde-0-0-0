private with Ada.Containers.Vectors;
private with Memor;

limited with Concorde.Empires;
limited with Concorde.Systems;

with Concorde.Components;
with Concorde.Modules;
with Concorde.Objects;

private with Newton.Flight;

package Concorde.Ships is

   type Root_Ship_Type is
     new Concorde.Objects.Root_Named_Object_Type with private;

   function Long_Name (Ship : Root_Ship_Type'Class) return String;

   function Short_Description (Ship : Root_Ship_Type'Class) return String;

   function Alive
     (Ship : Root_Ship_Type'Class)
      return Boolean;

   function Owner
     (Ship : Root_Ship_Type'Class)
      return access constant Concorde.Empires.Root_Empire_Type'Class;

   function System
     (Ship : Root_Ship_Type'Class)
      return access constant Concorde.Systems.Root_Star_System_Type'Class;

   function Has_Destination
     (Ship : Root_Ship_Type'Class)
      return Boolean;

   function Destination
     (Ship : Root_Ship_Type'Class)
      return access constant Concorde.Systems.Root_Star_System_Type'Class
     with Pre => Ship.Has_Destination;

   procedure Set_System
     (Ship : in out Root_Ship_Type'Class;
      System : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class);

   procedure Set_Owner
     (Ship   : in out Root_Ship_Type'Class;
      New_Owner : not null access constant
        Concorde.Empires.Root_Empire_Type'Class);

   procedure Set_Destination
     (Ship   : in out Root_Ship_Type'Class;
      System : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class);

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

   type Ship_Type is access constant Root_Ship_Type'Class;

   function Count_Ships
     (Test : not null access function
        (Ship : Ship_Type)
      return Boolean)
      return Natural;

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

   type Root_Ship_Type is
     new Concorde.Objects.Root_Named_Object_Type with
      record
         Identity              : String (1 .. 6);
         Owner                 : access constant
           Concorde.Empires.Root_Empire_Type'Class;
         System_Reference      : Memor.Database_Reference;
         Dest_Reference        : Memor.Database_Reference;
         Alive                 : Boolean;
         Structure             : Module_Vectors.Vector;
         Size                  : Size_Type;
--         Empty_Mass            : Non_Negative_Real;
         Current_Damage        : Unit_Real := 0.0;
         Current_Shields       : Unit_Real := 0.0;
         Location              : Newton.Flight.Vector_3;
         Velocity              : Newton.Flight.Vector_3;
         Orientation           : Newton.Flight.Matrix_3;
      end record;

   package Ship_Vectors is
     new Ada.Containers.Vectors (Positive, Ship_Type);

   overriding function Object_Database
     (Ship : Root_Ship_Type)
      return Memor.Root_Database_Type'Class;

end Concorde.Ships;
