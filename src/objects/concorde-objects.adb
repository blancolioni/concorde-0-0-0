with Concorde.Localisation;

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

   ------------------
   -- Check_Loaded --
   ------------------

   procedure Check_Loaded (Item : Root_Object_Type'Class) is

      procedure Perform_Load
        (RW_Item : not null access Memor.Root_Record_Type'Class);

      ------------------
      -- Perform_Load --
      ------------------

      procedure Perform_Load
        (RW_Item : not null access Memor.Root_Record_Type'Class)
      is
      begin
         Root_Object_Type'Class (RW_Item.all).Load;
         Root_Object_Type'Class (RW_Item.all).Loaded := True;
      end Perform_Load;

   begin
      if not Item.Loaded then
         Item.Object_Database.Update (Item.Reference, Perform_Load'Access);
      end if;
   end Check_Loaded;

   ----------------
   -- Identifier --
   ----------------

   overriding function Identifier
     (Item : Root_Object_Type)
      return String
   is
      Class_Item : Root_Object_Type'Class renames
                     Root_Object_Type'Class (Item);
      Db : Memor.Memor_Database renames
             Class_Item.Object_Database;
   begin
      return Db.Database_Class_Name & "-"
        & Memor.To_String (Class_Item.Reference);
   end Identifier;

   ---------------
   -- Local_Tag --
   ---------------

   function Local_Tag
     (Item : Root_Localised_Object_Type'Class)
      return String
   is
   begin
      return Item.Local_Tag.all;
   end Local_Tag;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Item : Root_Localised_Object_Type)
      return String
   is
   begin
      return Concorde.Localisation.Local_Name
        (Root_Localised_Object_Type'Class (Item).Identifier);
   end Name;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Item : Root_User_Named_Object_Type)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Object_Name);
   end Name;

   -------------------
   -- Set_Local_Tag --
   -------------------

   procedure Set_Local_Tag
     (Item      : in out Root_Localised_Object_Type'Class;
      Local_Tag : String)
   is
   begin
      Item.Local_Tag := new String'(Local_Tag);
   end Set_Local_Tag;

   --------------
   -- Set_Name --
   --------------

   overriding procedure Set_Name
     (Item : in out Root_User_Named_Object_Type;
      Name : String)
   is
   begin
      Item.Object_Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Set_Name;

end Concorde.Objects;
