with Concorde.Money;
with Concorde.Quantities;

with Concorde.Commodities.Db;
with Concorde.Installations.Db;

package body Concorde.Installations is

   ----------------------
   -- Add_Trade_Offers --
   ----------------------

   overriding procedure Add_Trade_Offers
     (Item   : not null access constant Root_Installation_Type;
      Market : in out Concorde.Trades.Trade_Interface'Class)
   is
      use Concorde.Quantities;

      procedure Add_Hub_Trade_Offer
        (Commodity : Concorde.Commodities.Commodity_Type);

      -------------------------
      -- Add_Hub_Trade_Offer --
      -------------------------

      procedure Add_Hub_Trade_Offer
        (Commodity : Concorde.Commodities.Commodity_Type)
      is
         Demand : constant Quantity := Market.Current_Demand (Commodity);
         Supply : constant Quantity := Market.Current_Supply (Commodity);
      begin
         if Demand > Supply then
            declare
               Sell_Quantity : constant Quantity :=
                                 Min (Item.Get_Quantity (Commodity),
                                      Demand - Supply);
            begin
               Item.Create_Sell_Offer
                 (Market, Commodity, Sell_Quantity, Concorde.Money.Zero);
            end;
         end if;
      end Add_Hub_Trade_Offer;

   begin

      for I in 1 .. Item.Facility.Worker_Count loop
         declare
            Commodity : constant Concorde.Commodities.Commodity_Type :=
                          Item.Facility.Worker_Skill (I).Commodity;
            Required  : constant Quantity :=
                          Item.Facility.Worker_Quantity (I);
         begin
            Item.Create_Buy_Offer
              (Market, Commodity, Required, Required);
         end;
      end loop;

      if Item.Is_Colony_Hub then
         Concorde.Commodities.Db.Scan (Add_Hub_Trade_Offer'Access);
      else
         if Item.Facility.Has_Output
           and then Item.Get_Quantity (Item.Facility.Output) > Zero
         then
            Item.Create_Sell_Offer
              (Market, Item.Facility.Output,
               Item.Get_Quantity (Item.Facility.Output),
               Concorde.Money.Zero);
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
