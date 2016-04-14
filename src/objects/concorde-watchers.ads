private with Ada.Containers.Doubly_Linked_Lists;

package Concorde.Watchers is

   type Watched_Object_Interface is limited interface;

   type Watcher_Interface is interface;

   procedure Add_Watcher
     (Object : in out Watched_Object_Interface;
      Watcher : not null access Watcher_Interface'Class)
   is abstract;

   procedure On_Object_Changed
     (Watcher : in out Watcher_Interface;
      Object  : Watched_Object_Interface'Class)
      is abstract;

   type Watcher_Type is access all Watcher_Interface'Class;

   type Watcher_List is private;

   procedure Add_Watcher
     (List    : in out Watcher_List;
      Watcher : not null access Watcher_Interface'Class);

   procedure Send_Changed
     (List   : Watcher_List;
      Object : Watched_Object_Interface'Class);

private

   package List_Of_Watchers is
     new Ada.Containers.Doubly_Linked_Lists (Watcher_Type);

   type Watcher_List is
      record
         List : List_Of_Watchers.List;
      end record;

end Concorde.Watchers;
