private with Ada.Strings.Unbounded;

with Memor;

package Concorde.Objects is

   type Root_Object_Type is abstract new Memor.Root_Record_Type with private;

   type Object_Type is access constant Root_Object_Type'Class;

   type Named_Object_Interface is limited interface;

   function Name (Item : Named_Object_Interface) return String is abstract;
   procedure Set_Name
     (Item : in out Named_Object_Interface;
      Name : String)
   is abstract;

   type Root_Named_Object_Type is
     abstract new Root_Object_Type
     and Memor.Identifier_Record_Type
     and Named_Object_Interface
   with private;

   overriding function Identifier
     (Item : Root_Named_Object_Type)
      return String;

   overriding function Name (Item : Root_Named_Object_Type) return String;
   overriding procedure Set_Name
     (Item : in out Root_Named_Object_Type;
      Name : String);

   type Named_Object_Type is access constant Root_Named_Object_Type'Class;

private

   type Root_Object_Type is abstract new Memor.Root_Record_Type
   with null record;

   type Root_Named_Object_Type is
     abstract new Root_Object_Type
     and Memor.Identifier_Record_Type
     and Named_Object_Interface with
      record
         Object_Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

end Concorde.Objects;
