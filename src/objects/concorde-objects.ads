private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;
private with WL.String_Maps;

with Memor;

with Concorde.Events;
with Concorde.Signals;
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

   procedure Check_Loaded (Item : Root_Object_Type'Class);

   procedure Load
     (Item : in out Root_Object_Type)
   is null;

   procedure Log
     (Item    : Root_Object_Type'Class;
      Message : String);

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

   type Massive_Object_Interface is limited interface;

   function Mass (Object : Massive_Object_Interface) return Non_Negative_Real
                  is abstract;

   type Object_Signal_Handler is access
     procedure (Event  : Concorde.Events.Root_Event_Type'Class;
                Object : not null access constant
                  Root_Object_Type'Class);

   procedure Add_Handler
     (Object  : in out Root_Object_Type'Class;
      Signal  : Concorde.Signals.Signal_Type;
      Handler : Object_Signal_Handler);

   type Object_Handler_Interface is interface;

   procedure Handle
     (Handler : in out Object_Handler_Interface;
      Event   : Concorde.Events.Root_Event_Type'Class;
      Object  : not null access constant Root_Object_Type'Class)
   is abstract;

   procedure Add_Handler
     (Object  : in out Root_Object_Type'Class;
      Signal  : Concorde.Signals.Signal_Type;
      Handler : not null access Object_Handler_Interface'Class);

   procedure Delete_Handler
     (Object  : in out Root_Object_Type'Class;
      Signal  : Concorde.Signals.Signal_Type;
      Handler : not null access Object_Handler_Interface'Class);

   procedure Signal
     (Object : not null access constant Root_Object_Type'Class;
      Sig    : Concorde.Signals.Signal_Type;
      Event  : Concorde.Events.Root_Event_Type'Class);

private

   type Object_Handler_Access is access all Object_Handler_Interface'Class;

   package Signal_Handler_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Object_Handler_Access);

   package Signal_Handler_Maps is
     new WL.String_Maps (Signal_Handler_Lists.List,
                         Signal_Handler_Lists."=");

   type Root_Object_Type is
     abstract new Memor.Root_Record_Type
     and Memor.Identifier_Record_Type
     and Concorde.Watchers.Watched_Object_Interface with
      record
         Watchers : Concorde.Watchers.Watcher_List;
         Handlers : Signal_Handler_Maps.Map;
         Loaded   : Boolean := False;
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

   type Base_Handler_Type is
     new Object_Handler_Interface with
      record
         Handler : Object_Signal_Handler;
      end record;

   overriding procedure Handle
     (Handler : in out Base_Handler_Type;
      Event   : Concorde.Events.Root_Event_Type'Class;
      Object  : not null access constant Root_Object_Type'Class);

end Concorde.Objects;
