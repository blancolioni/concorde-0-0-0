private with Ada.Strings.Unbounded;

with Memor;

package Concorde.Objects is

   type Root_Object_Type is abstract new Memor.Root_Record_Type with private;

   type Object_Type is access constant Root_Object_Type'Class;

   type Root_Named_Object_Type is
     abstract new Root_Object_Type
     and Memor.Identifier_Record_Type
   with private;

   overriding function Identifier
     (Item : Root_Named_Object_Type)
      return String;

   function Name (Item : Root_Named_Object_Type) return String;
   procedure Set_Name (Item : in out Root_Named_Object_Type;
                       Name : String);

   type Named_Object_Type is access constant Root_Named_Object_Type'Class;

private

   type Root_Object_Type is abstract new Memor.Root_Record_Type
   with null record;

   type Root_Named_Object_Type is
     abstract new Root_Object_Type
     and Memor.Identifier_Record_Type with
      record
         Object_Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

end Concorde.Objects;
