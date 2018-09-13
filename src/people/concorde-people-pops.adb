package body Concorde.People.Pops is

   function Income
     (Pop : Root_Pop_Type'Class)
      return Concorde.Network.Expression_Value;

   ---------------------
   -- Get_Field_Value --
   ---------------------

   overriding function Get_Field_Value
     (Pop   : Root_Pop_Type;
      Name  : String)
      return Concorde.Network.Expression_Value
   is
   begin
      if Name = "income" then
         return Income (Pop);
      else
         raise Constraint_Error with
           "unknown pop field: " & Name;
      end if;
   end Get_Field_Value;

   ------------
   -- Income --
   ------------

   function Income
     (Pop : Root_Pop_Type'Class)
      return Concorde.Network.Expression_Value
   is
      Result : Non_Negative_Real := Pop.Base_Income.Current_Base_Value;
   begin
      for Item of Pop.Groups loop
         Result := Result * (1.0 + Item.Income.Current_Value);
      end loop;
      return Concorde.Network.To_Expression_Value (Result * Real (Pop.Size));
   end Income;

   ------------------
   -- Is_Member_Of --
   ------------------

   function Is_Member_Of
     (Pop   : Root_Pop_Type'Class;
      Group : Concorde.People.Groups.Pop_Group)
      return Boolean
   is
      use type Concorde.People.Groups.Pop_Group;
   begin
      return (for some X of Pop.Groups => X.Group = Group);
   end Is_Member_Of;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Item : Root_Pop_Type)
      return Memor.Memor_Database
   is
      pragma Unreferenced (Item);
   begin
      return Db.Get_Database;
   end Object_Database;

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant Root_Pop_Type'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update;

end Concorde.People.Pops;
