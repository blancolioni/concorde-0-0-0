with Concorde.Commodities;
with Concorde.Random;

package body Concorde.People.Pops is

   ----------------------
   -- Add_Trade_Offers --
   ----------------------

   procedure Add_Trade_Offers
     (Item   : not null access constant Root_Pop_Type)
   is
      use Concorde.Commodities, Concorde.Quantities;
      Group : constant Concorde.People.Groups.Pop_Group :=
                Item.Wealth_Group;

      procedure Check_Need
        (Commodity : Concorde.Commodities.Commodity_Type;
         Need      : Non_Negative_Real);

      ----------------
      -- Check_Need --
      ----------------

      procedure Check_Need
        (Commodity : Concorde.Commodities.Commodity_Type;
         Need      : Non_Negative_Real)
      is
         Current : constant Quantity_Type := Item.Get_Quantity (Commodity);
         Required : constant Non_Negative_Real :=
                      Need * Non_Negative_Real (Item.Size);
      begin
         if Required < 1.0 and then Current = Zero then
            Item.Create_Bid (Commodity, Unit);
         elsif Required >= 1.0
           and then Current < To_Quantity (Real'Ceiling (Required))
         then
            Item.Create_Bid
              (Commodity,
               To_Quantity (Real'Ceiling (Required)) - Current);
         end if;
      end Check_Need;

   begin

      Group.Scan_Needs (Check_Need'Access);

      for Skill of Item.Skills loop
         if Item.Get_Quantity (Skill.Commodity) > Zero then
            Item.Create_Ask
              (Skill.Commodity, Item.Get_Quantity (Skill.Commodity));
         end if;
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

   -------------------------
   -- Execute_Consumption --
   -------------------------

   procedure Execute_Consumption
     (Pop : in out Root_Pop_Type'Class)
   is
      procedure Consume
        (Commodity : Concorde.Commodities.Commodity_Type;
         Need      : Non_Negative_Real);

      -------------
      -- Consume --
      -------------

      procedure Consume
        (Commodity : Concorde.Commodities.Commodity_Type;
         Need      : Non_Negative_Real)
      is
         use Concorde.Quantities;
         Total_Need : constant Non_Negative_Real :=
                        Need * Non_Negative_Real (Pop.Size);
         Base_Need  : constant Non_Negative_Real :=
                        Non_Negative_Real'Floor (Total_Need);
         Partial_Need : constant Non_Negative_Real :=
                          Total_Need - Base_Need;
         Need_Quantity : constant Quantity_Type :=
                           To_Quantity (Base_Need);
      begin
         Pop.Remove_Quantity
           (Commodity, Min (Pop.Get_Quantity (Commodity), Need_Quantity));
         if Pop.Get_Quantity (Commodity) > Zero
           and then Partial_Need > 0.0
           and then Concorde.Random.Unit_Random < Partial_Need
         then
            Pop.Remove_Quantity
              (Commodity, Unit);
         end if;
      end Consume;

   begin
      Pop.Wealth_Group.Scan_Needs (Consume'Access);
   end Execute_Consumption;

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
      return Concorde.Quantities.Quantity_Type
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
