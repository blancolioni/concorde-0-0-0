with Concorde.Money;
with Concorde.Quantities;

with Concorde.Commodities.Db;
with Concorde.Installations.Db;

package body Concorde.Installations is

   ----------------------
   -- Add_Trade_Offers --
   ----------------------

   overriding procedure Add_Trade_Offers
     (Item   : not null access constant Root_Installation_Type)
   is
      use Concorde.Quantities;

      procedure Add_Hub_Trade_Offer
        (Commodity : Concorde.Commodities.Commodity_Type);

      procedure Add_Sell_Offer
        (Commodity : Concorde.Commodities.Commodity_Type);

      -------------------------
      -- Add_Hub_Trade_Offer --
      -------------------------

      procedure Add_Hub_Trade_Offer
        (Commodity : Concorde.Commodities.Commodity_Type)
      is
         Demand : constant Quantity := Item.Market.Current_Demand (Commodity);
         Supply : constant Quantity := Item.Market.Current_Supply (Commodity);
      begin
         if not Commodity.Is_Set (Concorde.Commodities.Virtual) then
            if Demand > Supply then
               declare
                  Sell_Quantity : constant Quantity :=
                                    Min (Item.Get_Quantity (Commodity),
                                         Demand - Supply);
               begin
                  if Sell_Quantity > Zero then
                     Item.Create_Sell_Offer
                       (Commodity, Sell_Quantity, Concorde.Money.Zero);
                  end if;
               end;
            elsif Supply > Demand then
               declare
                  Buy_Quantity : constant Quantity :=
                                   Item.Get_Quantity (Commodity)
                                   + Supply - Demand;
               begin
                  Item.Create_Buy_Offer
                    (Commodity, Buy_Quantity, Buy_Quantity);
               end;
            end if;
         end if;
      end Add_Hub_Trade_Offer;

      --------------------
      -- Add_Sell_Offer --
      --------------------

      procedure Add_Sell_Offer
        (Commodity : Concorde.Commodities.Commodity_Type)
      is
      begin
         Item.Create_Sell_Offer
           (Commodity, Item.Get_Quantity (Commodity),
            Concorde.Money.Zero);
      end Add_Sell_Offer;

   begin

      for I in 1 .. Item.Facility.Input_Count loop
         declare
            Commodity : constant Concorde.Commodities.Commodity_Type :=
                          Item.Facility.Input_Commodity (I);
            Required  : constant Quantity :=
                          Item.Facility.Input_Quantity (I)
                          * Item.Facility.Capacity_Quantity;
         begin
            Item.Create_Buy_Offer
              (Commodity, Required, Required);
         end;
      end loop;

      for I in 1 .. Item.Facility.Worker_Count loop
         declare
            Commodity : constant Concorde.Commodities.Commodity_Type :=
                          Item.Facility.Worker_Skill (I).Commodity;
            Required  : constant Quantity :=
                          Item.Facility.Worker_Quantity (I);
         begin
            Item.Create_Buy_Offer
              (Commodity, Required, Required);
         end;
      end loop;

      if Item.Is_Colony_Hub then
         Concorde.Commodities.Db.Scan (Add_Hub_Trade_Offer'Access);
      else
         if Item.Facility.Has_Output
           and then Item.Get_Quantity (Item.Facility.Output) > Zero
         then
            Item.Create_Sell_Offer
              (Item.Facility.Output,
               Item.Get_Quantity (Item.Facility.Output),
               Concorde.Money.Zero);
         elsif Item.Facility.Is_Resource_Generator then
            declare
               function Match
                 (Commodity : Concorde.Commodities.Commodity_Type)
                  return Boolean
               is (Item.Facility.Can_Produce (Commodity)
                   and then Item.Get_Quantity (Commodity) > Zero);
            begin
               Concorde.Commodities.Db.Scan
                 (Match'Access,
                  Add_Sell_Offer'Access);
            end;
         end if;
      end if;
   end Add_Trade_Offers;

   --------------
   -- Facility --
   --------------

   function Facility
     (Installation : Root_Installation_Type'Class)
      return Concorde.Facilities.Facility_Type
   is
   begin
      return Installation.Facility;
   end Facility;

   -------------------
   -- Is_Colony_Hub --
   -------------------

   function Is_Colony_Hub
     (Installation : Root_Installation_Type'Class)
      return Boolean
   is
   begin
      case Installation.Facility.Class is
         when Concorde.Facilities.Colony_Hub =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Colony_Hub;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Item : Root_Installation_Type)
      return Memor.Root_Database_Type'Class
   is
      pragma Unreferenced (Item);
   begin
      return Db.Get_Database;
   end Object_Database;

   -----------
   -- Owner --
   -----------

   function Owner
     (Installation : Root_Installation_Type'Class)
      return access constant Concorde.Agents.Root_Agent_Type'Class
   is
   begin
      return Installation.Owner;
   end Owner;

end Concorde.Installations;
