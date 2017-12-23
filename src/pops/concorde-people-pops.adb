with WL.Money;

with Concorde.Commodities.Needs;
with Concorde.Worlds;

package body Concorde.People.Pops is

   ----------------------
   -- Add_Trade_Offers --
   ----------------------

   procedure Add_Trade_Offers
     (Pop : not null access constant Root_Pop_Type)
   is

      use WL.Money, WL.Quantities;
      use Concorde.Commodities;

      Remaining_Budget : Money_Type := Pop.Cash;

      procedure Check_Need
        (Level : Concorde.People.Groups.Need_Level);

      ----------------
      -- Check_Need --
      ----------------

      procedure Check_Need
        (Level : Concorde.People.Groups.Need_Level)
      is
         Need : Concorde.Commodities.Needs.Commodity_Needs;

         procedure Add_Need
           (Commodity : Concorde.Commodities.Commodity_Type;
            Quantity  : WL.Quantities.Quantity_Type);

         procedure Add_Bid
           (Commodity : Concorde.Commodities.Commodity_Type;
            Quantity  : WL.Quantities.Quantity_Type;
            Price     : WL.Money.Price_Type);

         -------------
         -- Add_Bid --
         -------------

         procedure Add_Bid
           (Commodity : Concorde.Commodities.Commodity_Type;
            Quantity  : WL.Quantities.Quantity_Type;
            Price     : WL.Money.Price_Type)
         is
         begin
            if Quantity > Zero then
               Pop.Create_Bid
                 (Commodity    => Commodity,
                  Bid_Quantity => Quantity,
                  Bid_Price    => Price);
            end if;
         end Add_Bid;

         --------------
         -- Add_Need --
         --------------

         procedure Add_Need
           (Commodity : Concorde.Commodities.Commodity_Type;
            Quantity  : WL.Quantities.Quantity_Type)
         is
         begin
            if Quantity > Pop.Get_Quantity (Commodity) then
               Concorde.Commodities.Needs.Add_Need
                 (Need      => Need,
                  Commodity => Commodity,
                  Quantity  => Quantity - Pop.Get_Quantity (Commodity),
                  Price     => Pop.Create_Bid_Price (Commodity));
            end if;
         end Add_Need;

      begin
         Concorde.Commodities.Needs.Set_Budget
           (Need, Remaining_Budget);
         Pop.Group.Scan_Needs
              (Level, Pop.Size_Quantity, Add_Need'Access);
         Concorde.Commodities.Needs.Scan_Needs
           (Need, Add_Bid'Access);

         declare
            Total_Cost : constant Money_Type :=
                           Concorde.Commodities.Needs.Total_Cost (Need);

         begin
            pragma Assert (Total_Cost <= Remaining_Budget);
            Remaining_Budget := Remaining_Budget - Total_Cost;
         end;

      end Check_Need;

   begin
      for Level in Concorde.People.Groups.Need_Level loop
         Check_Need (Level);
         exit when Remaining_Budget <= Zero;
      end loop;

      if Pop.Group.Unemployment
        and then Pop.Get_Quantity (Pop.Group.Work_Commodity) > Zero
      then
         Pop.Create_Ask
           (Commodity    => Pop.Group.Work_Commodity,
            Ask_Quantity => Pop.Get_Quantity (Pop.Group.Work_Commodity));
      end if;

   end Add_Trade_Offers;

   -------------------------
   -- Execute_Consumption --
   -------------------------

   procedure Execute_Consumption
     (Pop : in out Root_Pop_Type'Class)
   is

      use WL.Quantities;

      Total_Required : Quantity_Type;
      Total_Consumed : Quantity_Type;

      procedure Consume
        (Commodity : Concorde.Commodities.Commodity_Type;
         Required  : WL.Quantities.Quantity_Type);

      procedure Consume
        (Commodity : Concorde.Commodities.Commodity_Type;
         Required  : WL.Quantities.Quantity_Type)
      is
         Consumed : constant Quantity_Type :=
                      Min (Required, Pop.Get_Quantity (Commodity));
      begin
         Total_Required := Total_Required + Required;
         Total_Consumed := Total_Consumed + Consumed;
         Pop.Remove_Quantity (Commodity, Consumed);
      end Consume;

   begin
      for Level in Concorde.People.Groups.Need_Level loop

         Total_Required := Zero;
         Total_Consumed := Zero;

         Pop.Group.Scan_Needs
           (Level    => Level,
            Size     => Pop.Size_Quantity,
            Process  => Consume'Access);

         Pop.Consumption (Level) :=
           Consumption_Record'
             (Total_Needed   => Total_Required,
              Total_Consumed => Total_Consumed);

      end loop;
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
