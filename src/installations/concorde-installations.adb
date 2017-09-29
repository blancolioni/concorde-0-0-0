with Concorde.Commodities;
with Concorde.Money;
with Concorde.Quantities;

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
                          Item.Market.Current_Local_Demand (Commodity);
         Local_Supply : constant Quantity_Type :=
                          Item.Market.Current_Local_Supply (Commodity);
         Demand : constant Quantity_Type :=
                    Item.Market.Current_Demand (Commodity);
         Supply : constant Quantity_Type :=
                    Item.Market.Current_Supply (Commodity);
      begin
         if not Commodity.Is_Set (Concorde.Commodities.Virtual) then
            if Local_Demand > Supply then
               declare
                  Sell_Quantity : constant Quantity_Type :=
                                    Min (Item.Get_Quantity (Commodity),
                                         Demand - Supply);

                  procedure Update_Import_Demand
                    (Market : in out Concorde.Trades.Trade_Interface'Class);

                  --------------------------
                  -- Update_Import_Demand --
                  --------------------------

                  procedure Update_Import_Demand
                    (Market : in out Concorde.Trades.Trade_Interface'Class)
                  is
                  begin
                     Market.Add_Import_Demand
                       (Commodity, Local_Demand - Supply);
                  end Update_Import_Demand;

               begin
                  if Sell_Quantity > Zero then
                     Item.Create_Sell_Offer
                       (Commodity, Sell_Quantity, Concorde.Money.Zero);
                  end if;
                  Item.Market.Update (Update_Import_Demand'Access);
               end;
            elsif Local_Supply > Demand then
               declare
                  Buy_Quantity : constant Quantity_Type :=
                                   Item.Get_Quantity (Commodity)
                                   + Supply - Demand;

                  procedure Update_Export_Supply
                    (Market : in out Concorde.Trades.Trade_Interface'Class);

                  --------------------------
                  -- Update_Export_Supply --
                  --------------------------

                  procedure Update_Export_Supply
                    (Market : in out Concorde.Trades.Trade_Interface'Class)
                  is
                  begin
                     Market.Add_Export_Supply
                       (Commodity, Local_Supply - Demand);
                  end Update_Export_Supply;

               begin
                  Item.Create_Buy_Offer
                    (Commodity, Buy_Quantity, Buy_Quantity);
                  Item.Market.Update (Update_Export_Supply'Access);
               end;
            end if;
         end if;
      end Add_Hub_Trade_Offer;

      --------------------------
      -- Add_Port_Trade_Offer --
      --------------------------

      procedure Add_Port_Trade_Offer
        (Commodity : Concorde.Commodities.Commodity_Type)
      is
         Import_Demand : constant Quantity_Type :=
                           Item.Market.Current_Import_Demand (Commodity);
         Export_Supply : constant Quantity_Type :=
                           Item.Market.Current_Export_Supply (Commodity);
         In_Stock      : constant Quantity_Type :=
                           Item.Get_Quantity (Commodity);
      begin
         if Import_Demand > Zero and then In_Stock > Zero then
            Item.Create_Sell_Offer
              (Commodity, Min (Import_Demand, In_Stock),
               Concorde.Money.Adjust (Item.Get_Value (Commodity), 1.1));
         elsif Export_Supply > Zero then
            Item.Create_Buy_Offer
              (Commodity => Commodity,
               Desired   => Export_Supply,
               Minimum   => Export_Supply);
         end if;
      end Add_Port_Trade_Offer;

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
            Required  : constant Quantity_Type :=
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
            Required  : constant Quantity_Type :=
                          Item.Facility.Worker_Quantity (I);
         begin
            Item.Create_Buy_Offer
              (Commodity, Required, Required);
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
            Item.Create_Sell_Offer
              (Item.Facility.Output,
               Item.Get_Quantity (Item.Facility.Output),
               Concorde.Money.Zero);
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
