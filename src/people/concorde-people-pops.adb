package body Concorde.People.Pops is

   --------------------------
   -- Current_Income_Total --
   --------------------------

   function Current_Income_Total
     (Pop : Root_Pop_Type'Class)
      return Non_Negative_Real
   is
      Result : Non_Negative_Real := Pop.Base_Income.Current_Base_Value;
   begin
      for Item of Pop.Groups loop
         Result := Result * (1.0 + Item.Income.Current_Value);
      end loop;
      return Result * Real (Pop.Size);
   end Current_Income_Total;

   ------------------
   -- Daily_Budget --
   ------------------

   overriding function Daily_Budget
     (Pop       : Root_Pop_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Unit_Real
   is
      pragma Unreferenced (Pop);
   begin
      return Concorde.Commodities.Pop_Max_Budget (Commodity);
   end Daily_Budget;

   -----------------
   -- Daily_Needs --
   -----------------

   overriding function Daily_Needs
     (Pop       : Root_Pop_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Non_Negative_Real
   is
      pragma Unreferenced (Pop);
   begin
      return Concorde.Commodities.Pop_Daily_Needs (Commodity);
   end Daily_Needs;

   ------------------
   -- Daily_Supply --
   ------------------

   overriding function Daily_Supply
     (Pop       : Root_Pop_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Non_Negative_Real
   is
      pragma Unreferenced (Pop, Commodity);
   begin
      return 0.0;
   end Daily_Supply;

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
         return Concorde.Network.To_Expression_Value
           (Pop.Current_Income_Total);
      elsif Name = "size" then
         return Concorde.Network.To_Expression_Value
           (Non_Negative_Real (Pop.Size));
      else
         raise Constraint_Error with
           "unknown pop field: " & Name;
      end if;
   end Get_Field_Value;

   ---------------
   -- Has_Field --
   ---------------

   overriding function Has_Field
     (Pop   : Root_Pop_Type;
      Name  : String)
      return Boolean
   is
      pragma Unreferenced (Pop);
   begin
      return Name = "income";
   end Has_Field;

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

   ------------------
   -- Update_Agent --
   ------------------

   overriding procedure Update_Agent
     (Pop            : not null access constant Root_Pop_Type;
      Perform_Update : not null access
        procedure (Agent : in out Concorde.Agents.Root_Agent_Type'Class))
   is
   begin
      Perform_Update (Pop.Update);
   end Update_Agent;

end Concorde.People.Pops;
