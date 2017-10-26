with WL.Heaps;

with Concorde.Real_Images;
with Concorde.Worlds;

with Concorde.Logs;

package body Concorde.Installations is

   type Consumption_Record is
      record
         Commodity : Concorde.Commodities.Commodity_Type;
         Cost_Per  : WL.Money.Money_Type;
         Input_Per : WL.Quantities.Quantity_Type;
         Have      : WL.Quantities.Quantity_Type;
      end record;

   package Consumption_Queues is
     new WL.Heaps (WL.Money.Money_Type, Consumption_Record,
                   WL.Money.">");

   ----------------------
   -- Add_Trade_Offers --
   ----------------------

   procedure Add_Trade_Offers
     (Item   : not null access constant Root_Installation_Type)
   is
      use WL.Quantities;

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
         Supply       : constant Quantity_Type :=
                          Local_Supply +
                          Item.Market.Get_Daily_Quantity
                            (Commodity, Concorde.Trades.Import_Supply);
         Current_Ask  : constant Quantity_Type :=
                          Item.Current_Ask_Quantity (Commodity);
         Current_Bid  : constant Quantity_Type :=
                          Item.Current_Bid_Quantity (Commodity);
         Log_Path     : constant String :=
                          Item.Current_World.Name
                          & "/hub"
                          & "/offers"
                          & "/" & Commodity.Identifier;
      begin
         if not Commodity.Is_Set (Concorde.Commodities.Virtual)
           and then (Demand > Zero or else Supply > Zero)
         then
            Concorde.Logs.Log_Fields
              (Log_Path,
               Image (Local_Demand)
               & "," & Image (Local_Supply)
               & "," & Image (Demand)
               & "," & Image (Supply)
               & "," & Image (Current_Bid)
               & "," & Image (Current_Ask));

            Item.Log_Trade
              (Commodity.Name
               & ": local demand: " & Image (Local_Demand)
               & "; local supply: " & Image (Local_Supply)
               & "; total demand: " & Image (Demand)
               & "; total supply: " & Image (Supply)
               & "; currently buying " & Image (Current_Bid)
               & "; currently selling " & Image (Current_Ask));

            if Local_Demand > Supply + Current_Ask then
               declare
                  Sell_Quantity : constant Quantity_Type :=
                                    Min (Item.Get_Quantity (Commodity),
                                         Scale
                                           (Demand - Supply - Current_Ask,
                                            0.5));
               begin
                  if Sell_Quantity > Zero then
                     Item.Create_Ask
                       (Commodity, Sell_Quantity);
                  end if;
               end;
            elsif Local_Supply > Demand + Current_Bid then
               declare
                  Buy_Quantity : constant Quantity_Type :=
                                   Min (Item.Available_Quantity,
                                        Scale
                                          (Supply - Demand - Current_Bid,
                                           0.1));
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
         if Item.Current_Ask_Quantity (Commodity)
           < Item.Get_Quantity (Commodity)
         then
            Item.Create_Ask
              (Commodity,
               Item.Get_Quantity (Commodity)
               - Item.Current_Ask_Quantity (Commodity));
         end if;
      end Add_Sell_Offer;

   begin

      for I in 1 .. Item.Facility.Input_Count loop
         if Item.Facility.Simple_Input (I) then
            declare
               Commodity : constant Concorde.Commodities.Commodity_Type :=
                             Item.Facility.Input_Commodity (I);
               Required  : constant Quantity_Type :=
                             Item.Facility.Input_Quantity (I)
                             * Item.Facility.Capacity_Quantity;
               Have      : constant Quantity_Type :=
                             Item.Get_Quantity (Commodity)
                             + Item.Current_Bid_Quantity (Commodity);
            begin
               if Required > Have then
                  Item.Create_Bid
                    (Commodity, Required - Have);
               end if;
            end;
         elsif Item.Facility.Choice_Input (I) then
            declare
               use WL.Money;
               Cheapest_Item  : Concorde.Commodities.Commodity_Type;
               Cheapest_Price : Price_Type := Zero;
               Cheapest_Cost  : Money_Type := Zero;
               Cheapest_Reqd  : Quantity_Type := Zero;
            begin
               for J in 1 .. Item.Facility.Input_Choice_Count (I) loop
                  declare
                     Input : constant Concorde.Commodities.Commodity_Type :=
                               Item.Facility.Input_Choice_Commodity (I, J);
                     Need  : constant Quantity_Type :=
                               Item.Facility.Input_Choice_Quantity (I, J)
                               * Item.Facility.Capacity_Quantity;
                     Have  : constant Quantity_Type :=
                               Item.Get_Quantity (Input)
                               + Item.Current_Bid_Quantity (Input);
                     Reqd  : constant Quantity_Type :=
                               (if Need < Have then Zero
                                else Need - Have);
                     Price : constant Price_Type :=
                               Item.Mean_Price_Belief (Input);
                     Cost  : constant Money_Type :=
                               Total (Price, Reqd);
                  begin
                     if J = 1 or else Cost < Cheapest_Cost
                       or else
                         (Cost = Cheapest_Cost
                          and then Price > Cheapest_Price)
                     then
                        Cheapest_Item := Input;
                        Cheapest_Price := Price;
                        Cheapest_Cost := Cost;
                        Cheapest_Reqd := Reqd;
                        exit when Cheapest_Reqd = Zero;
                     end if;
                  end;
               end loop;

               if Cheapest_Reqd > Zero then
                  Item.Create_Bid
                    (Cheapest_Item, Cheapest_Reqd);
               end if;
            end;
         else
            raise Constraint_Error with
            Item.Short_Name
              & "cannot understand input rules for input"
              & I'Img;
         end if;
      end loop;

      for I in 1 .. Item.Facility.Worker_Count loop
         declare
            Commodity : constant Concorde.Commodities.Commodity_Type :=
                          Item.Facility.Worker_Skill (I).Commodity;
            Required  : constant Quantity_Type :=
                          Item.Facility.Worker_Quantity (I);
            Have      : constant Quantity_Type :=
                          Item.Get_Quantity (Commodity)
                          + Item.Current_Bid_Quantity (Commodity);

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
           and then Item.Get_Quantity (Item.Facility.Output)
           > Item.Current_Ask_Quantity (Item.Facility.Output)
         then
            Item.Create_Ask
              (Item.Facility.Output,
               Item.Get_Quantity (Item.Facility.Output)
               - Item.Current_Ask_Quantity (Item.Facility.Output));
         elsif Item.Facility.Is_Resource_Generator then
            for Commodity of Concorde.Commodities.All_Commodities loop
               if Item.Facility.Can_Produce (Commodity)
                 and then Item.Get_Quantity (Commodity) >
                 Item.Current_Ask_Quantity (Commodity)
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
      Quantity  : WL.Quantities.Quantity_Type;
      Wage      : WL.Money.Price_Type)
   is
      New_Employee : constant Employee_Record :=
                       Employee_Record'
                         (Pop            =>
                            Concorde.People.Pops.Pop_Type
                            (Employee),
                          Size           => Quantity,
                          Skill          =>
                            Concorde.People.Skills.Get (Commodity.Name),
                          Wage           => Wage,
                          Contract_Days  => 30,
                          Days_Remaining => 30);
   begin
      Employer.Update.Employees.Append (New_Employee);
   end Execute_Hire;

   ------------------------
   -- Execute_Production --
   ------------------------

   procedure Execute_Production
     (Installation : in out Root_Installation_Type'Class)
   is
      use WL.Quantities;
      use Concorde.Commodities;
      Facility        : constant Concorde.Facilities.Facility_Type :=
                          Installation.Facility;
      Production_Cost : WL.Money.Money_Type :=
                          WL.Money.Zero;
      Raw_Capacity       : constant Quantity_Type :=
                             Facility.Capacity_Quantity;

      procedure Consume_Input
        (Input_Index : Positive;
         Throughput  : Unit_Real);

      function Get_Input_Throughput
        (Input_Index : Positive)
         return Unit_Real;

      -------------------
      -- Consume_Input --
      -------------------

      procedure Consume_Input
        (Input_Index : Positive;
         Throughput  : Unit_Real)
      is
         use WL.Money;
         Queue : Consumption_Queues.Heap;
         Capacity : constant Quantity_Type :=
                      Scale (Raw_Capacity, Float (Throughput));
      begin
         if Facility.Simple_Input (Input_Index) then
            declare
               Commodity : constant Commodity_Type :=
                             Facility.Input_Commodity (Input_Index);
               Cost_Per  : constant Money_Type :=
                             Total
                               (Installation.Get_Average_Price (Commodity),
                                Facility.Input_Quantity (Input_Index));
            begin
               Queue.Insert
                 (Cost_Per,
                  Consumption_Record'
                    (Commodity => Commodity,
                     Cost_Per  => Cost_Per,
                     Input_Per => Facility.Input_Quantity (Input_Index),
                     Have      => Installation.Get_Quantity (Commodity)));
            end;
         else
            for I in 1 .. Facility.Input_Choice_Count (Input_Index) loop
               declare
                  Commodity : constant Commodity_Type :=
                                Facility.Input_Choice_Commodity
                                  (Input_Index, I);
                  Cost_Per  : constant Money_Type :=
                                Total
                                  (Installation.Get_Average_Price (Commodity),
                                   Facility.Input_Choice_Quantity
                                     (Input_Index, I));
               begin
                  Queue.Insert (Cost_Per,
                                Consumption_Record'
                                  (Commodity => Commodity,
                                   Cost_Per  => Cost_Per,
                                   Input_Per =>
                                     Facility.Input_Choice_Quantity
                                       (Input_Index, I),
                                   Have      =>
                                     Installation.Get_Quantity (Commodity)));
               end;
            end loop;
         end if;

         declare
            Remaining : Unit_Real := Throughput;
         begin
            while not Queue.Is_Empty and then Remaining > 0.0 loop
               declare
                  Item : constant Consumption_Record :=
                           Queue.Maximum_Element;
                  Required : constant Quantity_Type :=
                               Scale (Capacity, Float (Remaining))
                               * Item.Input_Per;
                  Cost     : Money_Type;
               begin
                  Queue.Delete_Maximum;

                  if Required <= Item.Have then
                     Remaining := 0.0;
                     Cost :=
                       Total (Installation.Get_Average_Price (Item.Commodity),
                              Required);
                     Installation.Remove_Quantity
                       (Item.Commodity, Required);
                  else
                     Cost := Installation.Get_Value (Item.Commodity);
                     Remaining :=
                       Real'Max
                         (0.0,
                          Remaining
                          - Unit_Real (To_Float (Item.Have)
                            / To_Float (Required)));
                     Installation.Remove_Quantity
                       (Item.Commodity, Item.Have);
                  end if;

                  Production_Cost := Production_Cost + Cost;
               end;
            end loop;
         end;

      end Consume_Input;

      --------------------------
      -- Get_Input_Throughput --
      --------------------------

      function Get_Input_Throughput
        (Input_Index : Positive)
         return Unit_Real
      is
      begin
         if Facility.Simple_Input (Input_Index) then
            declare
               Commodity : constant Commodity_Type :=
                             Facility.Input_Commodity (Input_Index);
               Required  : constant Quantity_Type :=
                             Facility.Input_Quantity (Input_Index)
                             * Raw_Capacity;
               Available : constant Quantity_Type :=
                             Installation.Get_Quantity (Commodity);
            begin
               if Available > Required then
                  return 1.0;
               else
                  return Unit_Real
                    (To_Float (Available)  / To_Float (Required));
               end if;
            end;

         else
            declare
               Throughput : Non_Negative_Real := 0.0;
            begin
               for I in 1 .. Facility.Input_Choice_Count (Input_Index) loop
                  declare
                     Commodity : constant Commodity_Type :=
                                   Facility.Input_Choice_Commodity
                                     (Input_Index, I);
                     Required  : constant Quantity_Type :=
                                   Facility.Input_Choice_Quantity
                                     (Input_Index, I)
                                   * Raw_Capacity;
                     Available : constant Quantity_Type :=
                                   Installation.Get_Quantity (Commodity);
                  begin
                     if Available > Required then
                        Throughput := 1.0;
                     else
                        Throughput := Throughput
                          + Unit_Real
                          (To_Float (Available) / To_Float (Required));
                     end if;
                     if Throughput >= 1.0 then
                        return 1.0;
                     end if;
                  end;
               end loop;
               return Throughput;
            end;
         end if;
      end Get_Input_Throughput;

   begin

      if Facility.Has_Output
        and then Facility.Output.Is_Set (Virtual)
      then
         Installation.Set_Quantity
           (Facility.Output, WL.Quantities.Zero, WL.Money.Zero);
      end if;

      declare
         use WL.Money;
         Throughput : Unit_Real := 1.0;
      begin
         for Worker of Installation.Employees loop
            Production_Cost := Production_Cost
              + Total (Worker.Wage, Worker.Size);
         end loop;

         for I in 1 .. Facility.Worker_Count loop
            declare
               Commodity : constant Commodity_Type :=
                             Facility.Worker_Skill (I).Commodity;
               Available : constant Quantity_Type :=
                             Installation.Get_Quantity (Commodity);
               Required  : constant Quantity_Type :=
                             Facility.Worker_Quantity (I);
            begin
               if Available < Required then
                  Throughput :=
                    Unit_Real'Min
                      (Real (To_Float (Available) / To_Float (Required)),
                       Throughput);
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
                  T : constant Unit_Real :=
                        Get_Input_Throughput (Input_Index);
               begin
                  Throughput := Unit_Real'Min (Throughput, T);
               end;
            end loop;

--                 declare
--                    Commodity : constant Commodity_Type :=
--                                  Facility.Input_Commodity (Input_Index);
--                    Required  : constant Quantity_Type :=
--                                  Facility.Input_Quantity (Input_Index)
--                                  * Raw_Capacity;
--                    Available : constant Quantity_Type :=
--                                  Installation.Get_Quantity (Commodity);
--                 begin
--                    if Available < Required then
--                       Throughput :=
--                         Unit_Real'Min
--                        (Real (To_Float (Available) / To_Float (Required)),
--                            Throughput);
--                       Effective_Capacity :=
--                         Min (Effective_Capacity,
--                           Scale_Down (Raw_Capacity, Available, Required));
--                    end if;
--                 end;
--              end loop;

         end if;

         declare
            Effective_Capacity : constant Quantity_Type :=
                                   Scale (Raw_Capacity, Float (Throughput));
         begin
            if Throughput < 1.0 then
               Installation.Log_Production
                 ("throughput limited to "
                  & Concorde.Real_Images.Approximate_Image
                    (Throughput * 100.0)
                  & "%; effective capacity " & Image (Effective_Capacity));
            end if;

            if Effective_Capacity > Zero then

               for Input_Index in 1 .. Facility.Input_Count loop
                  Consume_Input (Input_Index, Throughput);
               end loop;

               if Facility.Is_Farm then
                  Installation.Add_Quantity
                    (Facility.Output,
                     Effective_Capacity,
                     Production_Cost);

                  Installation.Log_Production
                    ("produces "
                     & Image (Effective_Capacity)
                     & " "
                     & Facility.Output.Name
                     & " for "
                     & Image (Production_Cost)
                     & "; stock now "
                     & Image (Installation.Get_Quantity (Facility.Output)));

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
                     Generated     : Quantity_Type;
                  begin
                     Concorde.Worlds.Get_Sector_Resource
                       (Installation.Current_Location,
                        Resource, Concentration, Accessibility);

                     Factor :=
                       (Accessibility + Concentration);
                     Generated :=
                       Scale (Effective_Capacity, Float (Factor));

                     Installation.Log_Production
                       ("generates "
                        & Image (Generated)
                        & " "
                        & Resource.Name
                        & " for "
                        & Image (Production_Cost));

                     if Generated + Installation.Total_Quantity
                       > Installation.Maximum_Quantity
                     then
                        declare
                           Lose : constant Quantity_Type :=
                                    Generated
                                      + Installation.Total_Quantity
                                    - Installation.Maximum_Quantity;
                        begin
                           Installation.Log_Production
                             ("loses " & Image (Lose)
                              & " due to full storage");
                           Generated := Generated - Lose;
                        end;
                     end if;

                     Installation.Add_Quantity
                       (Resource,
                        Generated, Production_Cost);

                     Installation.Log_Production
                       ("minimum price per " & Resource.Name & " now "
                        & Image (Installation.Get_Average_Price (Resource)));

                  end;
               end if;
            end if;
         end;

         --              Conflict.Commodities.Add
         --                (Installation.Reference, Installation.Production,
         --                 Effective_Capacity, Production_Cost);
         --           end if;
      end;

      declare
         Workers : Employee_Lists.List;
         Changed : Boolean := False;
      begin
         for Worker of Installation.Employees loop
            Worker.Days_Remaining := Worker.Days_Remaining - 1;
            if Worker.Days_Remaining = 0 then
               Installation.Remove_Quantity
                 (Worker.Skill.Commodity,
                  Installation.Get_Quantity (Worker.Skill.Commodity));
               Worker.Pop.Update.Add_Quantity
                 (Worker.Skill.Commodity, Worker.Size,
                  WL.Money.Total (Worker.Wage, Worker.Size));
               Changed := True;
            else
               Workers.Append (Worker);
            end if;
         end loop;

         if Changed then
            Installation.Employees := Workers;
         end if;
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
      Employees : Employee_Lists.List;
      Changed   : Boolean := False;
   begin
      for Worker of Installation.Employees loop
         declare
            use WL.Money;
            Cost : constant WL.Money.Money_Type :=
                     WL.Money.Total (Worker.Wage, Worker.Size);
         begin
            Installation.Require_Cash (Cost);
            if Installation.Cash >= Cost then
               Installation.Log_Wages (Worker.Pop, Worker.Size, Worker.Wage);
               Installation.Remove_Cash (Cost);
               Worker.Pop.Update.Add_Cash (Cost);
               Employees.Append (Worker);
            else
               Installation.Log
                 ("insufficient cash to pay workers; needed "
                  & Show (Cost) & "; have " & Show (Installation.Cash));
               Installation.Remove_Quantity
                 (Worker.Skill.Commodity,
                  Installation.Get_Quantity (Worker.Skill.Commodity));
               Worker.Pop.Update.Add_Quantity
                 (Worker.Skill.Commodity, Worker.Size,
                  WL.Money.Total (Worker.Wage, Worker.Size));
               Changed := True;
            end if;
         end;
      end loop;
      if Changed then
         Installation.Employees := Employees;
      end if;
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
