with Concorde.Commodities;
with Concorde.Money;

with Concorde.People.Pops.Db;

package body Concorde.People.Pops is

   ----------------------
   -- Add_Trade_Offers --
   ----------------------

   overriding procedure Add_Trade_Offers
     (Item   : not null access constant Root_Pop_Type)
   is
      use Concorde.Commodities;
      use type Concorde.Quantities.Quantity;
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
   begin
      for Skill of Item.Skills loop
         Item.Create_Sell_Offer
           (Skill.Commodity, Item.Size_Quantity, Concorde.Money.Zero);
      end loop;
      for Need of Needs loop
         Item.Create_Buy_Offer (Need, Desired, Minimum);
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
      return Pop.Groups.Element (Group.Reference);
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

end Concorde.People.Pops;
