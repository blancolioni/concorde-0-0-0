private with Ada.Containers.Vectors;

limited with Concorde.Empires;
limited with Concorde.Systems;

--  with Concorde.Components;
with Concorde.Modules;
with Concorde.Objects;

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
      return access Concorde.Empires.Root_Empire_Type'Class;

   function System
     (Ship : Root_Ship_Type'Class)
      return access constant Concorde.Systems.Root_Star_System_Type'Class;

   function Destination
     (Ship : Root_Ship_Type'Class)
      return access constant Concorde.Systems.Root_Star_System_Type'Class;

   function Has_Destination
     (Ship : Root_Ship_Type'Class)
      return Boolean
   is (Ship.Destination /= null);

   procedure Set_System
     (Ship : in out Root_Ship_Type'Class;
      System : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class);

   procedure Set_Destination
     (Ship : in out Root_Ship_Type'Class;
      System : access constant Concorde.Systems.Root_Star_System_Type'Class);

   function Damage
     (Ship : Root_Ship_Type'Class)
      return Unit_Real;

   procedure Hit
     (Ship : in out Root_Ship_Type'Class;
      Damage : Natural);

   function Acceleration
     (Ship : Root_Ship_Type'Class)
      return Non_Negative_Real
   is (5.0);
   --  m/s/s

   function Turn
     (Ship : Root_Ship_Type'Class)
      return Non_Negative_Real
   is (1.0);
   --  degrees/s

   function Size
     (Ship : Root_Ship_Type'Class)
      return Natural;

   procedure Update_Power
     (Ship : in out Root_Ship_Type'Class);

   function Get_Weapon_Modules
     (Ship : Root_Ship_Type'Class)
      return Concorde.Modules.Array_Of_Modules;

   type Ship_Type is access all Root_Ship_Type'Class;

   function Count_Ships
     (Test : not null access function
        (Ship : Ship_Type)
      return Boolean)
      return Natural;

private

   package Module_Vectors is
     new Ada.Containers.Vectors
       (Positive, Concorde.Modules.Module_Type, Concorde.Modules."=");

   type Root_Ship_Type is
     new Concorde.Objects.Root_Named_Object_Type with
      record
         Identity    : String (1 .. 5);
         Owner       : access Concorde.Empires.Root_Empire_Type'Class;
         System      : access constant
           Concorde.Systems.Root_Star_System_Type'Class;
         Destination : access constant
           Concorde.Systems.Root_Star_System_Type'Class;
         Alive       : Boolean;
         Structure   : Module_Vectors.Vector;
         Size        : Natural;
         Empty_Mass  : Natural;
      end record;

   package Ship_Vectors is
     new Ada.Containers.Vectors (Positive, Ship_Type);

   Ship_Vector : Ship_Vectors.Vector;

end Concorde.Ships;
