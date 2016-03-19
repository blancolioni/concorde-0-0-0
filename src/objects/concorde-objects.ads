private with Ada.Strings.Unbounded;

package Concorde.Objects is

   type Root_Object_Type is abstract tagged limited private;

   type Object_Type is access constant Root_Object_Type'Class;

   type Root_Named_Object_Type is abstract limited new Root_Object_Type
   with private;

   function Name (Item : Root_Named_Object_Type) return String;
   procedure Set_Name (Item : in out Root_Named_Object_Type;
                       Name : String);

   type Named_Object_Type is access constant Root_Named_Object_Type'Class;

private

   type Root_Object_Type is abstract tagged limited null record;

   type Root_Named_Object_Type is abstract limited new Root_Object_Type with
      record
         Object_Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

end Concorde.Objects;
