with Concorde.People.Communities;
with Concorde.People.Individuals;

package body Concorde.Ministries is

   ---------------------
   -- Add_Budget_Item --
   ---------------------

   procedure Add_Budget_Item
     (Budget : in out Ministry_Budget;
      Value  : Concorde.Money.Money_Type)
   is
   begin
      Budget.List.Append
        ((Explicit_Value, Value));
   end Add_Budget_Item;

   ---------------------
   -- Add_Budget_Item --
   ---------------------

   procedure Add_Budget_Item
     (Budget     : in out Ministry_Budget;
      Pop_Group  : Concorde.People.Groups.Pop_Group;
      Per_Capita : Concorde.Money.Price_Type)
   is
   begin
      Budget.List.Append
        ((Pop_Group_Based, Pop_Group, Per_Capita));
   end Add_Budget_Item;

   ---------------
   -- Add_Power --
   ---------------

   overriding procedure Add_Power
     (Ministry   : in out Root_Ministry_Type;
      Power      : Concorde.Powers.Power_Type)
   is
   begin
      Ministry.Log ("new power: " & Power.Show);
      Ministry.Powers.Add_Power (Power);
   end Add_Power;

   function Daily_Budget
     (Ministry : Root_Ministry_Type'Class)
      return Concorde.Money.Money_Type
   is
      use Concorde.Money;
   begin
      return Budget : Money_Type := Zero do
         for Item of Ministry.Daily_Budget loop
            case Item.Class is
               when Explicit_Value =>
                  Budget := Budget + Item.Value;
               when Pop_Group_Based =>
                  Budget := Budget
                    + Total (Item.Per_Capita,
                             Ministry.Community.Group_Population
                               (Item.Pop_Group));
            end case;
         end loop;
      end return;
   end Daily_Budget;

   ----------------
   -- Daily_Work --
   ----------------

   function Daily_Work
     (Ministry : Root_Ministry_Type'Class)
      return Duration
   is
      Result : constant Duration := 0.0;

      procedure Add_Work
        (Power : Concorde.Powers.Power_Type);

      --------------
      -- Add_Work --
      --------------

      procedure Add_Work
        (Power : Concorde.Powers.Power_Type)
      is
      begin
         null;
--           Result := Result
--             + Power.Daily_Work (Ministry.Headquarters.Community);
      end Add_Work;

   begin
      Ministry.Powers.Scan_Powers (Add_Work'Access);
      return Result;
   end Daily_Work;

   ---------------------
   -- Delegated_Power --
   ---------------------

   overriding procedure Delegate_Power
     (Ministry   : in out Root_Ministry_Type;
      Power      : Concorde.Powers.Power_Type;
      To         : not null access constant
        Concorde.Bureaucracy.Bureaucratic_Interface'Class)
   is
   begin
      Ministry.Delegated_Powers.Append
        ((Power_Holder.To_Holder (Power), To));
   end Delegate_Power;

   ------------------
   -- Delegated_To --
   ------------------

   overriding function Delegated_To
     (Ministry : Root_Ministry_Type;
      Power    : Concorde.Powers.Power_Type)
      return not null access constant
     Concorde.Bureaucracy.Bureaucratic_Interface'Class
   is
      use type Concorde.Powers.Power_Type;
   begin
      for Rec of Ministry.Delegated_Powers loop
         if Rec.Power.Element = Power then
            return Rec.Delegated_To;
         end if;
      end loop;
      raise Constraint_Error with
        Ministry.Short_Name & " does not delegate"
        & Concorde.Powers.Show (Power);
   end Delegated_To;

   ------------------
   -- Remove_Power --
   ------------------

   overriding procedure Remove_Power
     (Ministry   : in out Root_Ministry_Type;
      Power      : Concorde.Powers.Power_Type)
   is
   begin
      Ministry.Powers.Remove_Power (Power);
   end Remove_Power;

   -----------------
   -- Scan_Powers --
   -----------------

   overriding procedure Scan_Powers
     (Ministry   : Root_Ministry_Type;
      Process    : not null access
        procedure (Power : Concorde.Powers.Power_Type))
   is
   begin
      Ministry.Powers.Scan_Powers (Process);
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

   ------------------
   -- Update_Agent --
   ------------------

   overriding procedure Update_Agent
     (Ministry       : not null access constant Root_Ministry_Type;
      Perform_Update : not null access
        procedure (Agent : in out Concorde.Agents.Root_Agent_Type'Class))
   is
   begin
      Perform_Update (Ministry.Update);
   end Update_Agent;

end Concorde.Ministries;
