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
