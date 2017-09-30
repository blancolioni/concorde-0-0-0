with Concorde.Commodities;
with Concorde.Quantities;

package body Concorde.Installations is

   ----------------------
   -- Add_Trade_Offers --
   ----------------------

   procedure Add_Trade_Offers
     (Item   : not null access constant Root_Installation_Type)
   is
      use Concorde.Quantities;

      procedure Add_Hub_Trade_Offer
        (Commodity : Concorde.Commodities.Commodity_Type);

      procedure Add_Port_Trade_Offer
        (Commodity : Concorde.Commodities.Commodity_Type);

      procedure Add_Sell_Offer
        (Commodity : Concorde.Commodities.Commodity_Type);

      -------------------------
      -- Add_Hub_Trade_Offer --
      -------------------------

      procedure Add_Hub_Trade_Offer
        (Commodity : Concorde.Commodities.Commodity_Type)
      is
         Local_Demand : constant Quantity_Type :=
                          Item.Market.Get_Daily_Quantity
                            (Commodity, Concorde.Trades.Local_Demand);
         Local_Supply : constant Quantity_Type :=
                          Item.Market.Get_Daily_Quantity
                            (Commodity, Concorde.Trades.Local_Supply);
         Demand : constant Quantity_Type :=
                          Item.Market.Get_Daily_Quantity
                            (Commodity, Concorde.Trades.Total_Demand);
         Supply : constant Quantity_Type :=
                          Item.Market.Get_Daily_Quantity
                            (Commodity, Concorde.Trades.Total_Supply);
      begin
         if not Commodity.Is_Set (Concorde.Commodities.Virtual) then
            if Local_Demand > Supply then
               declare
                  Sell_Quantity : constant Quantity_Type :=
                                    Min (Item.Get_Quantity (Commodity),
                                         Demand - Supply);
               begin
                  if Sell_Quantity > Zero then
                     Item.Create_Ask
                       (Commodity, Sell_Quantity);
                  end if;
               end;
            elsif Local_Supply > Demand then
               declare
                  Buy_Quantity : constant Quantity_Type :=
                                   Item.Get_Quantity (Commodity)
                                   + Supply - Demand;
               begin
                  Item.Create_Bid
                    (Commodity, Buy_Quantity);
               end;
            end if;
         end if;
      end Add_Hub_Trade_Offer;

      --------------------------
      -- Add_Port_Trade_Offer --
      --------------------------

      procedure Add_Port_Trade_Offer
        (Commodity : Concorde.Commodities.Commodity_Type)
      is null;
--           Local_Demand  : constant Quantity_Type :=
--                             Item.Market.Get_Daily_Quantity
--                               (Commodity, Concorde.Trades.Local_Demand);
--           Local_Supply  : constant Quantity_Type :=
--                             Item.Market.Get_Daily_Quantity
--                               (Commodity, Concorde.Trades.Local_Supply);
--           In_Stock      : constant Quantity_Type :=
--                             Item.Get_Quantity (Commodity);
--        begin
--           if Local_Demand > Local_Supply and then In_Stock > Zero then
--              Item.Create_Ask
--                (Commodity, Min (Local_Demand - Local_Supply, In_Stock));
--           elsif Local_Supply > Local_Demand then
--              Item.Create_Bid (Commodity, Export_Supply);
--           end if;
--        end Add_Port_Trade_Offer;

      --------------------
      -- Add_Sell_Offer --
      --------------------

      procedure Add_Sell_Offer
        (Commodity : Concorde.Commodities.Commodity_Type)
      is
      begin
         Item.Create_Ask
           (Commodity, Item.Get_Quantity (Commodity));
      end Add_Sell_Offer;

   begin

      for I in 1 .. Item.Facility.Input_Count loop
         declare
            Commodity : constant Concorde.Commodities.Commodity_Type :=
                          Item.Facility.Input_Commodity (I);
            Required  : constant Quantity_Type :=
                          Item.Facility.Input_Quantity (I)
                          * Item.Facility.Capacity_Quantity;
         begin
            Item.Create_Bid
              (Commodity, Required);
         end;
      end loop;

      for I in 1 .. Item.Facility.Worker_Count loop
         declare
            Commodity : constant Concorde.Commodities.Commodity_Type :=
                          Item.Facility.Worker_Skill (I).Commodity;
            Required  : constant Quantity_Type :=
                          Item.Facility.Worker_Quantity (I);
         begin
            Item.Create_Bid
              (Commodity, Required);
         end;
      end loop;

      if Item.Is_Colony_Hub then
         for Commodity of Concorde.Commodities.All_Commodities loop
            Add_Hub_Trade_Offer (Commodity);
         end loop;
      elsif Item.Is_Port then
         for Commodity of Concorde.Commodities.All_Commodities loop
            Add_Port_Trade_Offer (Commodity);
         end loop;
      else
         if Item.Facility.Has_Output
           and then Item.Get_Quantity (Item.Facility.Output) > Zero
         then
            Item.Create_Ask
              (Item.Facility.Output,
               Item.Get_Quantity (Item.Facility.Output));
         elsif Item.Facility.Is_Resource_Generator then
            for Commodity of Concorde.Commodities.All_Commodities loop
               if Item.Facility.Can_Produce (Commodity)
                 and then Item.Get_Quantity (Commodity) > Zero
               then
                  Add_Sell_Offer (Commodity);
               end if;
            end loop;
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

   -------------
   -- Is_Port --
   -------------

   function Is_Port
     (Installation : Root_Installation_Type'Class)
      return Boolean
   is
   begin
      case Installation.Facility.Class is
         when Concorde.Facilities.Port =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Port;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Item : Root_Installation_Type)
      return Memor.Memor_Database
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

   --------------------
   -- Remove_Manager --
   --------------------

   procedure Remove_Manager
     (Installation : in out Root_Installation_Type'Class)
   is
   begin
      Installation.Manager := null;
   end Remove_Manager;

   -----------------
   -- Set_Manager --
   -----------------

   procedure Set_Manager
     (Installation : in out Root_Installation_Type'Class;
      Manager      : not null access constant
        Concorde.People.Individuals.Root_Individual_Type'Class)
   is
   begin
      Installation.Manager := Manager;
   end Set_Manager;

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant Root_Installation_Type'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update;

end Concorde.Installations;
