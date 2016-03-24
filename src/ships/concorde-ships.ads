private with Ada.Containers.Vectors;

limited with Concorde.Empires;
limited with Concorde.Systems;

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

   type Ship_Type is access all Root_Ship_Type'Class;

   function Count_Ships
     (Test : not null access function
        (Ship : Ship_Type)
      return Boolean)
      return Natural;

private

   type Root_Ship_Type is
     new Concorde.Objects.Root_Named_Object_Type with
      record
         Owner       : access Concorde.Empires.Root_Empire_Type'Class;
         System      : access constant
           Concorde.Systems.Root_Star_System_Type'Class;
         Destination : access constant
           Concorde.Systems.Root_Star_System_Type'Class;
         Alive       : Boolean;
         HP          : Natural;
         Max_HP      : Positive;
      end record;

   package Ship_Vectors is
     new Ada.Containers.Vectors (Positive, Ship_Type);

   Ship_Vector : Ship_Vectors.Vector;

end Concorde.Ships;
