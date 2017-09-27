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
      Handler.Manager.On_Activated (Event.Time_Stamp);
   end Handle;

end Concorde.Managers;
