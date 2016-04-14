private with Ada.Strings.Unbounded;

with Memor;

with Concorde.Watchers;

package Concorde.Objects is

   type Root_Object_Type is
     abstract new Memor.Root_Record_Type
     and Concorde.Watchers.Watched_Object_Interface
   with private;

   overriding procedure Add_Watcher
     (Object  : in out Root_Object_Type;
      Watcher : not null access Concorde.Watchers.Watcher_Interface'Class);

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

   type Root_Object_Type is
     abstract new Memor.Root_Record_Type
     and Concorde.Watchers.Watched_Object_Interface with
      record
         Watchers : Concorde.Watchers.Watcher_List;
      end record;

   overriding procedure After_Change
     (Object : Root_Object_Type);

   type Root_Named_Object_Type is
     abstract new Root_Object_Type
     and Memor.Identifier_Record_Type
     and Named_Object_Interface with
      record
         Object_Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

end Concorde.Objects;
