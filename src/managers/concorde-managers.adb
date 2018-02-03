with Ada.Containers.Doubly_Linked_Lists;

package body Concorde.Managers is

   package Manager_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Manager_Type);

   Manager_List : Manager_Lists.List;

   --------------
   -- Activate --
   --------------

   procedure Activate (Manager : not null access Root_Manager_Type'Class) is
   begin
      Manager.Active := True;
      Manager_List.Append (Manager);
   end Activate;

   -------------------
   -- Add_Work_Item --
   -------------------

   procedure Add_Work_Item
     (Manager : in out Root_Manager_Type'Class;
      Item    : not null access constant
        Concorde.Work.Root_Work_Item'Class)
   is
   begin
      Manager.Work_Queue.Insert
        (Item.Priority, Concorde.Work.Work_Item (Item));
   end Add_Work_Item;

   ----------------
   -- Deactivate --
   ----------------

   procedure Deactivate (Manager : not null access Root_Manager_Type'Class) is
      Position : Manager_Lists.Cursor := Manager_List.Find (Manager);
   begin
      pragma Assert (Manager_Lists.Has_Element (Position));
      Manager_List.Delete (Position);
      Manager.Active := False;
   end Deactivate;

   ------------
   -- Handle --
   ------------

   overriding procedure Handle
     (Handler : in out Object_Activated_Handler;
      Event   : Concorde.Events.Root_Event_Type'Class;
      Object  : not null access constant
        Concorde.Objects.Root_Object_Type'Class)
   is
      pragma Unreferenced (Object);
   begin
      Handler.Manager.Time := Event.Time_Stamp;
      Handler.Manager.On_Activated;
   end Handle;

end Concorde.Managers;
