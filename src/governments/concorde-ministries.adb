with Concorde.People.Individuals;
with Concorde.Powers.Execution;

package body Concorde.Ministries is

   ---------------
   -- Add_Power --
   ---------------

   overriding procedure Add_Power
     (Ministry   : in out Root_Ministry_Type;
      Power      : Concorde.Powers.Power_Type)
   is
   begin
      Ministry.Powers.Insert (Power);
   end Add_Power;

   ----------------
   -- Daily_Work --
   ----------------

   function Daily_Work
     (Ministry : Root_Ministry_Type'Class)
      return Duration
   is
      Result : Duration := 0.0;

      procedure Add_Work
        (Power : Concorde.Powers.Power_Type);

      --------------
      -- Add_Work --
      --------------

      procedure Add_Work
        (Power : Concorde.Powers.Power_Type)
      is
      begin
         Result := Result
           + Concorde.Powers.Execution.Daily_Work
           (Power, Ministry.Headquarters.World);
      end Add_Work;

   begin
      Ministry.Powers.Scan_Powers (Add_Work'Access);
      return Result;
   end Daily_Work;

   ------------------
   -- Remove_Power --
   ------------------

   overriding procedure Remove_Power
     (Ministry   : in out Root_Ministry_Type;
      Power      : Concorde.Powers.Power_Type)
   is
   begin
      Ministry.Powers.Remove (Power);
   end Remove_Power;

   -----------------
   -- Scan_Powers --
   -----------------

   overriding procedure Scan_Powers
     (Item    : Root_Ministry_Type;
      Process : not null access
        procedure (Power : Concorde.Powers.Power_Type))
   is
   begin
      Item.Powers.Scan_Powers (Process);
   end Scan_Powers;

   ------------------
   -- Set_Minister --
   ------------------

   procedure Set_Minister
     (Ministry : in out Root_Ministry_Type'Class;
      Minister : access constant
        Concorde.People.Individuals.Root_Individual_Type'Class)
   is
   begin
      Ministry.Minister := Minister;
   end Set_Minister;

   --------------
   -- Set_Name --
   --------------

   overriding procedure Set_Name
     (Ministry : in out Root_Ministry_Type;
      New_Name : String)
   is
   begin
      Ministry.Name := Ada.Strings.Unbounded.To_Unbounded_String (New_Name);
   end Set_Name;

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant Root_Ministry_Type'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update;

end Concorde.Ministries;
