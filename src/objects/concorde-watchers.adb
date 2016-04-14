package body Concorde.Watchers is

   -----------------
   -- Add_Watcher --
   -----------------

   procedure Add_Watcher
     (List    : in out Watcher_List;
      Watcher : not null access Watcher_Interface'Class)
   is
   begin
      List.List.Append (Watcher_Type (Watcher));
   end Add_Watcher;

   ------------------
   -- Send_Changed --
   ------------------

   procedure Send_Changed
     (List   : Watcher_List;
      Object : Watched_Object_Interface'Class)
   is
   begin
      for Watcher of List.List loop
         Watcher.On_Object_Changed (Object);
      end loop;
   end Send_Changed;

end Concorde.Watchers;
