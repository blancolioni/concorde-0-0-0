with Concorde.Real_Images;
with Concorde.Worlds;

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
         Demand       : constant Quantity_Type :=
                          Local_Demand +
                          Item.Market.Get_Daily_Quantity
                            (Commodity, Concorde.Trades.Export_Demand);
         Supply : constant Quantity_Type :=
                          Item.Market.Get_Daily_Quantity
                            (Commodity, Concorde.Trades.Import_Supply);
      begin
         if not Commodity.Is_Set (Concorde.Commodities.Virtual) then
            Item.Log_Trade
              (Commodity.Name
               & ": local demand: " & Image (Local_Demand)
               & "; local supply: " & Image (Local_Supply)
               & "; total demand: " & Image (Demand)
               & ": total supply: " & Image (Supply));
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
                                   Min (Item.Available_Quantity,
                                        Supply - Demand);
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
            Have      : constant Quantity_Type :=
                          Item.Get_Quantity (Commodity);
         begin
            if Required > Have then
               Item.Create_Bid
                 (Commodity, Required - Have);
            end if;
         end;
      end loop;

      for I in 1 .. Item.Facility.Worker_Count loop
         declare
            Commodity : constant Concorde.Commodities.Commodity_Type :=
                          Item.Facility.Worker_Skill (I).Commodity;
            Required  : constant Quantity_Type :=
                          Item.Facility.Worker_Quantity (I);
            Have      : constant Quantity_Type :=
                          Item.Get_Quantity (Commodity);
         begin
            if Required > Have then
               Item.Create_Bid
                 (Commodity, Required - Have);
            end if;
         end;
      end loop;

      if Item.Is_Colony_Hub then
         for Commodity of Concorde.Commodities.Trade_Commodities loop
            Add_Hub_Trade_Offer (Commodity);
         end loop;
      elsif Item.Is_Port then
         for Commodity of Concorde.Commodities.Trade_Commodities loop
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

   ------------------
   -- Execute_Hire --
   ------------------

   overriding procedure Execute_Hire
     (Employer  : not null access constant Root_Installation_Type;
      Employee  : not null access constant
        Concorde.Trades.Trader_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Wage      : Concorde.Money.Price_Type)
   is
      New_Employee : constant Employee_Record :=
                       Employee_Record'
                         (Pop   => Concorde.People.Pops.Pop_Type (Employee),
                          Size  => Quantity,
                          Skill =>
                            Concorde.People.Skills.Get (Commodity.Name),
                          Wage  => Wage);
   begin
      Employer.Update.Employees.Append (New_Employee);
   end Execute_Hire;

   ------------------------
   -- Execute_Production --
   ------------------------

   procedure Execute_Production
     (Installation : in out Root_Installation_Type'Class)
   is
      use Concorde.Commodities;
      Facility        : constant Concorde.Facilities.Facility_Type :=
                          Installation.Facility;
      Production_Cost : Concorde.Money.Money_Type :=
                          Money.Zero;
   begin

      if Facility.Has_Output
        and then Facility.Output.Is_Set (Virtual)
      then
         Installation.Set_Quantity
           (Facility.Output, Quantities.Zero, Money.Zero);
      end if;

      declare
         use Concorde.Money;
         use Concorde.Quantities;
         Raw_Capacity       : constant Quantity_Type :=
                                Facility.Capacity_Quantity;
         Throughput         : Unit_Real := 1.0;
         Effective_Capacity : Quantity_Type := Raw_Capacity;
      begin
         for I in 1 .. Facility.Worker_Count loop
            declare
               Commodity : constant Commodity_Type :=
                             Facility.Worker_Skill (I).Commodity;
               Available : constant Quantity_Type :=
                             Installation.Get_Quantity (Commodity);
               Required  : constant Quantity_Type :=
                             Facility.Worker_Quantity (I);
            begin
               Production_Cost :=
                 Production_Cost + Installation.Get_Value (Commodity);

               if Available < Required then
                  Throughput :=
                    Unit_Real'Min
                      (To_Real (Available) / To_Real (Required),
                       Throughput);
                  Effective_Capacity :=
                    Min (Effective_Capacity,
                         Scale_Down (Raw_Capacity, Available, Required));
                  Installation.Log_Production
                    (Image (Available) & " of "
                     & Image (Required)
                     & " " & Commodity.Name
                     & ": throughput = "
                     & Concorde.Real_Images.Approximate_Image
                       (Throughput * 100.0)
                     & "%");
               end if;
            end;
         end loop;

         if Facility.Has_Output then

            for Input_Index in 1 .. Facility.Input_Count loop
               declare
                  Commodity : constant Commodity_Type :=
                                Facility.Input_Commodity (Input_Index);
                  Required  : constant Quantity_Type :=
                                Facility.Input_Quantity (Input_Index)
                                * Raw_Capacity;
                  Available : constant Quantity_Type :=
                                Installation.Get_Quantity (Commodity);
               begin
                  if Available < Required then
                     Throughput :=
                       Unit_Real'Min
                         (To_Real (Available) / To_Real (Required),
                          Throughput);
                     Effective_Capacity :=
                       Min (Effective_Capacity,
                            Scale_Down (Raw_Capacity, Available, Required));
                  end if;
               end;
            end loop;

         end if;

         if Throughput < 1.0 then
            Installation.Log_Production
              ("throughput limited to "
               & Concorde.Real_Images.Approximate_Image
                 (Throughput * 100.0)
               & "%; effective capacity " & Image (Effective_Capacity));
         end if;

         if Effective_Capacity > Zero then

            for Input_Index in 1 .. Facility.Input_Count loop
               declare
                  Commodity : constant Commodity_Type :=
                                Facility.Input_Commodity (Input_Index);
                  Required  : constant Quantity_Type :=
                                Facility.Input_Quantity (Input_Index)
                                * Effective_Capacity;
                  Price_Per : constant Price_Type :=
                                Installation.Get_Average_Price (Commodity);
                  Cost      : constant Money_Type :=
                                Money.Total (Price_Per, Required);
               begin
                  Production_Cost := Production_Cost + Cost;
                  Installation.Remove_Quantity (Commodity, Required);
               end;
            end loop;

            if Facility.Is_Farm then
               Installation.Log_Production
                 ("produces "
                  & Image (Effective_Capacity)
                  & " "
                  & Facility.Output.Name
                  & " for "
                  & Image (Production_Cost));

               Installation.Add_Quantity
                 (Facility.Output,
                  Effective_Capacity,
                  Production_Cost);

            elsif Facility.Has_Output then
               Installation.Log_Production
                 ("produces "
                  & Image (Effective_Capacity)
                  & " "
                  & Facility.Output.Name
                  & " for "
                  & Image (Production_Cost));

               Installation.Add_Quantity
                 (Facility.Output,
                  Effective_Capacity,
                  Production_Cost);

            elsif Facility.Is_Resource_Generator then

               declare
                  Resource      : Concorde.Commodities.Commodity_Type;
                  Concentration : Unit_Real;
                  Accessibility : Unit_Real;
                  Factor        : Non_Negative_Real;
               begin
                  Concorde.Worlds.Get_Sector_Resource
                    (Installation.Current_Location,
                     Resource, Concentration, Accessibility);

                  Factor :=
                    (Accessibility + Concentration);
                  Effective_Capacity :=
                    Scale (Effective_Capacity, Factor);

                  Installation.Log_Production
                    ("generates "
                     & Image (Effective_Capacity)
                     & " "
                     & Resource.Name
                     & " for "
                     & Image (Production_Cost));

                  if Effective_Capacity + Installation.Total_Quantity
                    > Installation.Maximum_Quantity
                  then
                     declare
                        Lose : constant Quantity_Type :=
                                 Effective_Capacity
                                   + Installation.Total_Quantity
                                 - Installation.Maximum_Quantity;
                     begin
                        Installation.Log_Production
                          ("loses " & Image (Lose) & " due to full storage");
                        Effective_Capacity := Effective_Capacity - Lose;
                     end;
                  end if;

                  Installation.Add_Quantity
                    (Resource,
                     Effective_Capacity, Production_Cost);
               end;
            end if;
         end if;

         --              Conflict.Commodities.Add
         --                (Installation.Reference, Installation.Production,
         --                 Effective_Capacity, Production_Cost);
         --           end if;
      end;

   end Execute_Production;

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

   -----------------
   -- Pay_Workers --
   -----------------

   procedure Pay_Workers
     (Installation : in out Root_Installation_Type'Class)
   is
   begin
      for Worker of Installation.Employees loop
         declare
            Cost : constant Concorde.Money.Money_Type :=
                     Concorde.Money.Total (Worker.Wage, Worker.Size);
         begin
            Installation.Remove_Cash (Cost);
            Worker.Pop.Update.Add_Cash (Cost);
         end;
      end loop;
   end Pay_Workers;

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
