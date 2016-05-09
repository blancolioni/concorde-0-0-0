private with Ada.Strings.Unbounded;

with Memor;

with Concorde.Watchers;

package Concorde.Objects is

   type Root_Object_Type is
     abstract new Memor.Root_Record_Type
     and Memor.Identifier_Record_Type
     and Concorde.Watchers.Watched_Object_Interface
   with private;

   overriding function Identifier
     (Item : Root_Object_Type)
      return String;

   overriding procedure Add_Watcher
     (Object  : in out Root_Object_Type;
      Watcher : not null access Concorde.Watchers.Watcher_Interface'Class);

   type Object_Type is access constant Root_Object_Type'Class;

   type Named_Object_Interface is limited interface;

   function Name (Item : Named_Object_Interface) return String is abstract;

   type User_Named_Object_Interface is limited interface
     and Named_Object_Interface;

   procedure Set_Name
     (Item : in out User_Named_Object_Interface;
      Name : String)
   is abstract;

   type Root_User_Named_Object_Type is
     abstract new Root_Object_Type
     and User_Named_Object_Interface
   with private;

   overriding function Name
     (Item : Root_User_Named_Object_Type)
      return String;

   overriding procedure Set_Name
     (Item : in out Root_User_Named_Object_Type;
      Name : String);

   type User_Named_Object_Type is
     access constant Root_User_Named_Object_Type'Class;

   type Root_Localised_Object_Type is
     abstract new Root_Object_Type
     and Named_Object_Interface
   with private;

   overriding function Name
     (Item : Root_Localised_Object_Type)
      return String;

   function Local_Tag
     (Item : Root_Localised_Object_Type'Class)
      return String;

   procedure Set_Local_Tag
     (Item      : in out Root_Localised_Object_Type'Class;
      Local_Tag : String);

private

   type Root_Object_Type is
     abstract new Memor.Root_Record_Type
     and Memor.Identifier_Record_Type
     and Concorde.Watchers.Watched_Object_Interface with
      record
         Watchers : Concorde.Watchers.Watcher_List;
      end record;

   overriding procedure After_Change
     (Object : Root_Object_Type);

   type Root_Localised_Object_Type is
     abstract new Root_Object_Type
     and Named_Object_Interface
     and Memor.Identifier_Record_Type with
      record
         Local_Tag : access String;
      end record;

   overriding function Identifier
     (Item : Root_Localised_Object_Type)
      return String
   is (Item.Local_Tag.all);

   type Root_User_Named_Object_Type is
     abstract new Root_Object_Type
     and Memor.Identifier_Record_Type
     and User_Named_Object_Interface with
      record
         Object_Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

end Concorde.Objects;
