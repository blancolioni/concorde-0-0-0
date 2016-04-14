package body Concorde.Objects is

   -----------------
   -- Add_Watcher --
   -----------------

   overriding procedure Add_Watcher
     (Object  : in out Root_Object_Type;
      Watcher : not null access Concorde.Watchers.Watcher_Interface'Class)
   is
   begin
      Concorde.Watchers.Add_Watcher
        (Object.Watchers, Watcher);
   end Add_Watcher;

   ------------------
   -- After_Change --
   ------------------

   overriding procedure After_Change
     (Object : Root_Object_Type)
   is
   begin
      Concorde.Watchers.Send_Changed (Object.Watchers, Object);
   end After_Change;

   ----------------
   -- Identifier --
   ----------------

   overriding function Identifier
     (Item : Root_Named_Object_Type)
      return String
   is
      Class_Item : Root_Named_Object_Type'Class renames
                     Root_Named_Object_Type'Class (Item);
      Db : Memor.Root_Database_Type'Class renames
             Class_Item.Object_Database;
   begin
      return Db.Database_Class_Name & "-"
        & Memor.To_String (Class_Item.Reference);
   end Identifier;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Item : Root_Named_Object_Type)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Object_Name);
   end Name;

   --------------
   -- Set_Name --
   --------------

   overriding procedure Set_Name
     (Item : in out Root_Named_Object_Type;
      Name : String)
   is
   begin
      Item.Object_Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Set_Name;

end Concorde.Objects;
