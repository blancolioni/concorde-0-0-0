with Concorde.Commodities;
with Concorde.Money;

package body Concorde.People.Pops is

   ----------------------
   -- Add_Trade_Offers --
   ----------------------

   overriding procedure Add_Trade_Offers
     (Item   : not null access constant Root_Pop_Type)
   is
      use Concorde.Commodities, Concorde.Money, Concorde.Quantities;
      Group : constant Concorde.People.Groups.Pop_Group :=
                Item.Wealth_Group;
      Quality : constant Commodity_Quality := Group.Preferred_Quality;
      Needs : constant Array_Of_Commodities :=
                Concorde.Commodities.Get
                  (Consumer, Quality);
      Minimum : constant Concorde.Quantities.Quantity :=
                  Item.Size_Quantity;
      Desired : constant Concorde.Quantities.Quantity :=
                  Minimum * Quantities.To_Quantity (7.0);
      Min_Budget : Money_Type := Zero;
      Max_Budget : Money_Type := Zero;
      Income     : constant Money_Type :=
                     Total
                       (Item.Market.Current_Price
                          (Item.Skills.First_Element.Commodity),
                        Item.Size_Quantity);
      type Need_Orders is array (Needs'Range) of Quantity;
      Min_Order   : Need_Orders;
      Max_Order   : Need_Orders;
      Final_Order : Need_Orders;
   begin
      for I in Needs'Range loop
         declare
            Need  : constant Commodity_Type := Needs (I);
            Price : constant Price_Type :=
                      Item.Market.Current_Price (Need);
            Min   : constant Quantity :=
                      (if Minimum < Item.Get_Quantity (Need)
                       then Zero else Minimum - Item.Get_Quantity (Need));
            Max   : constant Quantity :=
                      (if Desired < Item.Get_Quantity (Need)
                       then Zero else Desired - Item.Get_Quantity (Need));
         begin
            Min_Order (I) := Min;
            Max_Order (I) := Max;
            Min_Budget := Min_Budget + Total (Price, Min);
            Max_Budget := Max_Budget + Total (Price, Max);
         end;
      end loop;

      declare
         Final_Budget : Money_Type;
      begin
         if Max_Budget <= Item.Cash then
            Final_Budget := Max_Budget;
            Final_Order := Max_Order;
         elsif Max_Budget < Income then
            if Min_Budget < Income then
               declare
                  Factor : constant Real :=
                             To_Real (Item.Cash) / To_Real (Max_Budget);
               begin
                  Final_Budget := Item.Cash;
                  for I in Needs'Range loop
                     Final_Order (I) := Scale (Max_Order (I), Factor);
                  end loop;
               end;
            else
               Final_Order := Min_Order;
               Final_Budget := Min_Budget;
            end if;
         else
            Final_Order := Min_Order;
            Final_Budget := Min_Budget;
         end if;
         Item.Log_Price
           ("min budget " & Image (Min_Budget)
            & "; max budget " & Image (Max_Budget)
            & "; cash " & Image (Item.Cash)
            & "; forecast income " & Image (Income)
            & "; final budget " & Image (Final_Budget));
      end;

      for Skill of Item.Skills loop
         Item.Create_Sell_Offer
           (Skill.Commodity, Item.Size_Quantity, Concorde.Money.Zero);
      end loop;

      for I in Needs'Range loop
         Item.Create_Buy_Offer (Needs (I), Final_Order (I), Final_Order (I));
      end loop;

   end Add_Trade_Offers;

   -----------------
   -- Affiliation --
   -----------------

   overriding function Affiliation
     (Pop   : Root_Pop_Type;
      Group : Concorde.People.Groups.Pop_Group)
      return Concorde.People.Groups.Affiliation_Range
   is
   begin
      return Pop.Groups.Get_Affiliation_Range (Group);
   end Affiliation;

   -------------------
   -- Before_Market --
   -------------------

   overriding procedure Before_Market
     (Pop : in out Root_Pop_Type)
   is
   begin
      for Skill of Pop.Skills loop
         Pop.Add_Quantity (Skill.Commodity, Pop.Size_Quantity,
                           Concorde.Money.Zero);
      end loop;
   end Before_Market;

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

   ----------
   -- Size --
   ----------

   function Size (Pop : Root_Pop_Type'Class) return Pop_Size is
   begin
      return Pop.Size;
   end Size;

   -------------------
   -- Size_Quantity --
   -------------------

   function Size_Quantity
     (Pop : Root_Pop_Type'Class)
      return Concorde.Quantities.Quantity
   is
   begin
      return Concorde.Quantities.To_Quantity (Real (Pop.Size));
   end Size_Quantity;

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
