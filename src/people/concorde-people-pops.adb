package body Concorde.People.Pops is

   ------------------
   -- Daily_Budget --
   ------------------

   overriding function Daily_Budget
     (Pop       : Root_Pop_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Money_Type
   is
      use Concorde.Money;
   begin
      return Adjust
        (Pop.Cash, Concorde.Commodities.Pop_Max_Budget (Commodity));
   end Daily_Budget;

   -----------------
   -- Daily_Needs --
   -----------------

   overriding function Daily_Needs
     (Pop       : Root_Pop_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Concorde.Quantities.Scale
        (Pop.Size_Quantity,
         Concorde.Commodities.Pop_Daily_Needs (Commodity));
   end Daily_Needs;

   ------------------
   -- Daily_Supply --
   ------------------

   overriding function Daily_Supply
     (Pop       : Root_Pop_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
   is
      use type Concorde.Commodities.Commodity_Type;
   begin
      if Pop.Group.Has_Commodity
        and then Pop.Group.Commodity = Commodity
      then
         return Pop.Size_Quantity;
      end if;
      return Concorde.Quantities.Zero;
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
      if Name = "size" then
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
