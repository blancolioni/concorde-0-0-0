package body Concorde.Objects is

   ----------
   -- Name --
   ----------

   function Name (Item : Root_Named_Object_Type) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Object_Name);
   end Name;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (Item : in out Root_Named_Object_Type;
      Name : String)
   is
   begin
      Item.Object_Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Set_Name;

end Concorde.Objects;
