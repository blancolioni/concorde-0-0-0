with WL.Heaps;

with Concorde.Contracts;
with Concorde.Government;
with Concorde.People.Groups;
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

   procedure New_Port_Contracts
     (Port      : not null access constant Root_Installation_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : WL.Quantities.Quantity_Type;
      Price     : WL.Money.Price_Type)
     with Pre => Port.Is_Port;

   ----------------------
   -- Add_Trade_Offers --
   ----------------------

   procedure Add_Trade_Offers
     (Item   : not null access constant Root_Installation_Type)
   is
      use WL.Quantities;

      Government : constant Concorde.Government.Government_Type :=
                     Concorde.Government.Government_Type
                       (Item.Government);

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
                          Item.Market.Current_Demand (Commodity);
         Local_Supply : constant Quantity_Type :=
                          Item.Market.Current_Supply (Commodity);
         Demand       : constant Quantity_Type := Local_Demand;
         Supply       : constant Quantity_Type := Local_Supply;
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
                  if Buy_Quantity > Zero then
                     Item.Create_Bid
                       (Commodity, Buy_Quantity);
                  end if;
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
         use WL.Money;
         use Concorde.Calendar;
         Local_Demand          : constant Quantity_Type :=
                                   Item.Market.Current_Demand (Commodity);
         Local_Supply          : constant Quantity_Type :=
                                   Item.Market.Current_Supply (Commodity);
         Current_Buy_Contracts : constant Quantity_Type :=
                                   Root_Installation_Type'Class (Item.all)
                                   .Contracted_To_Buy (Commodity);
         Quantity              : constant Quantity_Type :=
                                   Min
                                     (Item.Contract_Capacity
                                      - Item.Contracted_Quantity,
                                      Local_Demand - Local_Supply);
      begin

         if Quantity > Zero
           and then Clock - Start > Days (7)
           and then Current_Buy_Contracts < To_Quantity (100_000.0)
           and then Local_Demand > Scale (Local_Supply, 1.1)
           and then Local_Demand - Local_Supply
             >  To_Quantity (100.0) + Current_Buy_Contracts
         then
            declare
               Price    : constant Price_Type :=
                            Adjust_Price
                              (Item.Market.Current_Price
                                 (Commodity),
                               0.9);
            begin
               Item.Log ("seven day supply/demand/price for " & Commodity.Name
                         & " is "
                         & Show (Local_Supply)
                         & "/"
                         & Show (Local_Demand)
                         & "/"
                         & Show (Price));

               New_Port_Contracts
                 (Port      => Item,
                  Commodity => Commodity,
                  Quantity  => Quantity,
                  Price     => Price);

            end;
         end if;
         Item.Update.Close_Completed_Contracts;
      end Add_Port_Trade_Offer;

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
                             Item.Facility.Input_Quantity
                               (Item.Size, I);
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
                               Item.Facility.Input_Choice_Quantity
                                 (Item.Size, I, J);
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

      for I in 1 .. Item.Facility.Pop_Group_Count loop
         declare
            Group     : constant Concorde.People.Groups.Pop_Group :=
                          Item.Facility.Pop_Group (I);
            Commodity : constant Concorde.Commodities.Commodity_Type :=
                          Group.Work_Commodity;
            Required  : constant Quantity_Type :=
                          Item.Facility.Pop_Group_Quantity
                            (Item.Size, I);
            Have      : constant Quantity_Type :=
                          Item.Get_Quantity (Commodity)
                          + Item.Current_Bid_Quantity (Commodity);
         begin
            if (not Group.Is_Slave or else Government.Slavery_Allowed)
              and then Required > Have
            then
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
         Item.Log_Production
           ("current contracted quantity/capacity: "
            & Show (Item.Contracted_Quantity)
            & "/"
            & Show (Item.Contract_Capacity));
         if Item.Contracted_Quantity < Item.Contract_Capacity then
            for Commodity of Concorde.Commodities.Trade_Commodities loop
               Add_Port_Trade_Offer (Commodity);
            end loop;
         end if;

         Item.Scan_Stock
           (Add_Sell_Offer'Access);

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
      pragma Unreferenced (Commodity);
      New_Employee : constant Employee_Record :=
                       Employee_Record'
                         (Pop            =>
                            Concorde.People.Pops.Pop_Type
                            (Employee),
                          Size           => Quantity,
                          Wage           => Wage,
                          Contract_Days  => 30,
                          Days_Remaining => 30);
   begin
      Employer.Update.Employees.Append (New_Employee);
   end Execute_Hire;

   ------------------------
   -- Execute_Production --
   ------------------------

   overriding procedure Execute_Production
     (Installation : in out Root_Installation_Type)
   is
      use WL.Quantities;
      use Concorde.Commodities;
      Facility        : constant Concorde.Facilities.Facility_Type :=
                          Installation.Facility;
      World           : constant Concorde.Worlds.World_Type :=
                          Concorde.Worlds.World_Type
                            (Installation.Current_World);
      Production_Cost : WL.Money.Money_Type :=
                          WL.Money.Zero;

      Production_Size : constant WL.Quantities.Quantity_Type :=
                          Installation.Size;

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
--           Capacity : constant Quantity_Type :=
--                        Scale (Raw_Capacity, Float (Throughput));
      begin
         for I in 1 .. Facility.Input_Choice_Count (Input_Index) loop
            declare
               Commodity : constant Commodity_Type :=
                             Facility.Input_Choice_Commodity
                               (Input_Index, I);
               Quantity  : constant Quantity_Type :=
                             Facility.Input_Choice_Quantity
                               (Production_Size, Input_Index, I);
               Cost_Per  : constant Money_Type :=
                             Total
                               (Installation.Get_Average_Price (Commodity),
                                Quantity);
            begin
               Queue.Insert (Cost_Per,
                             Consumption_Record'
                               (Commodity => Commodity,
                                Cost_Per  => Cost_Per,
                                Input_Per => Quantity,
                                Have      =>
                                  Installation.Get_Quantity (Commodity)));
            end;
         end loop;

         declare
            Remaining : Unit_Real := Throughput;
         begin
            while not Queue.Is_Empty and then Remaining > 0.0 loop
               declare
                  Item : constant Consumption_Record :=
                           Queue.Maximum_Element;
                  Required : constant Quantity_Type :=
                               Scale (Item.Input_Per, Float (Remaining));
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
         Throughput : Non_Negative_Real := 0.0;
      begin
         for I in 1 .. Facility.Input_Choice_Count (Input_Index) loop
            declare
               Commodity : constant Commodity_Type :=
                             Facility.Input_Choice_Commodity
                               (Input_Index, I);
               Required  : constant Quantity_Type :=
                             Facility.Input_Choice_Quantity
                               (Installation.Size, Input_Index, I);
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
      end Get_Input_Throughput;

      use WL.Money;
      use Concorde.Facilities;

      Base_Production : constant Quantity_Type :=
                          Facility.Base_Output_Quantity
                            (Installation.Size, 1.0);
      Effective_Capacity : Quantity_Type := Base_Production;
      Input_Limit     : Unit_Real := 1.0;

   begin

      Installation.Log_Production
        ("size: " & Show (Installation.Size)
         & "; base production: "
         & Show (Base_Production));

      Installation.Efficiency :=
        (Concorde.Facilities.Input      => 1.0,
         Concorde.Facilities.Throughput => 0.0,
         Concorde.Facilities.Output     => 0.0);

      if Facility.Has_Output
        and then Facility.Output.Is_Set (Virtual)
      then
         Installation.Set_Quantity
           (Facility.Output, WL.Quantities.Zero, WL.Money.Zero);
      end if;

      for Worker of Installation.Employees loop
         Production_Cost := Production_Cost
           + Total (Worker.Wage, Worker.Size);
      end loop;

      for I in 1 .. Facility.Pop_Group_Count loop
         declare
            Commodity : constant Commodity_Type :=
                          Facility.Pop_Group (I).Work_Commodity;
            Required  : constant Quantity_Type :=
                          Facility.Pop_Group_Quantity
                            (Installation.Size, I);
            Available : constant Quantity_Type :=
                          Min (Required,
                               Installation.Get_Quantity (Commodity));
            Effect    : Non_Negative_Real renames
                          Installation.Efficiency
                            (Facility.Pop_Group_Effect (I));
         begin
            Effect := Effect
              + Non_Negative_Real
              (To_Float (Available) / To_Float (Required));
         end;
      end loop;

      if Installation.Facility.Is_Artisan then
         Installation.Efficiency (Concorde.Facilities.Throughput) := 1.0;
      end if;

      declare
         Throughput     : Non_Negative_Real renames
                            Installation.Efficiency
                              (Concorde.Facilities.Throughput);
         Infrastructure : constant Non_Negative_Real :=
                            Concorde.Worlds.Get_Sector_Infrastructure
                              (Installation.Current_Location);
      begin
         Throughput := Throughput
           * (1.0 + Infrastructure);
      end;

      declare
         Output : Non_Negative_Real renames
                    Installation.Efficiency (Concorde.Facilities.Output);
      begin
         Output := 1.0 + Output;
      end;

      for Input_Index in 1 .. Facility.Input_Count loop
         declare
            T : constant Unit_Real :=
                  Get_Input_Throughput (Input_Index);
         begin
            Input_Limit := Non_Negative_Real'Min (Input_Limit, T);
         end;
      end loop;

      if not Installation.Facility.Is_Artisan then
         declare
            Total_Population : WL.Quantities.Quantity_Type :=
                                 WL.Quantities.Zero;
            Owner_Population : WL.Quantities.Quantity_Type :=
                                 WL.Quantities.Zero;

            Efficiency       : Non_Negative_Real renames
                                 Installation.Efficiency
                                   (Facility.Owner_Effect);

            procedure Add_Population
              (Pop : Concorde.People.Pops.Pop_Type);

            --------------------
            -- Add_Population --
            --------------------

            procedure Add_Population
              (Pop : Concorde.People.Pops.Pop_Type)
            is
               use type Concorde.People.Groups.Pop_Group;
            begin
               Total_Population := Total_Population + Pop.Size_Quantity;
               if Pop.Group = Facility.Owner_Pop then
                  Owner_Population := Owner_Population + Pop.Size_Quantity;
               end if;
            end Add_Population;

         begin

            World.Scan_Pops (Add_Population'Access);

            Installation.Log_Production
              ("owner/total: " & Show (Owner_Population)
               & "/" & Show (Total_Population));

            Efficiency :=
              Efficiency
                + Facility.Owner_Effect_Factor
              * Real (To_Float (Owner_Population)
                      / To_Float (Total_Population));
         end;
      end if;

      Installation.Log_Production
        ("base: " & Show (Base_Production)
         & "; input"
         & Natural'Image
           (Natural (Installation.Efficiency (Input) * 100.0))
         & "% throughput"
         & Natural'Image
           (Natural (Installation.Efficiency (Throughput) * 100.0))
         & "% output"
         & Natural'Image
           (Natural (Installation.Efficiency (Output) * 100.0))
         & "% efficiency"
         & Natural'Image
           (Natural (Installation.Efficiency (Output) * 100.0))
         & "%");

      if Input_Limit > 0.0
        and then (for some X of Installation.Efficiency =>
                    X > 0.0)
      then

         for Input_Index in 1 .. Facility.Input_Count loop
            Consume_Input
              (Input_Index, Input_Limit * Installation.Efficiency (Input));
         end loop;

         Effective_Capacity :=
           Scale (Effective_Capacity,
                  Float
                    (Installation.Efficiency (Throughput)
                     * Installation.Efficiency (Output)
                     * Input_Limit));

         if Effective_Capacity > Installation.Available_Capacity then
            Installation.Log_Production
              ("production reduced from " & Show (Effective_Capacity)
               & " to " & Show (Installation.Available_Capacity)
               & " because of available capacity");

            Effective_Capacity := Installation.Available_Capacity;
         end if;

         if Effective_Capacity > Zero then
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
      end if;

      --              Conflict.Commodities.Add
      --                (Installation.Reference, Installation.Production,
      --                 Effective_Capacity, Production_Cost);
      --           end if;

      declare
         Workers : Employee_Lists.List;
         Changed : Boolean := False;
      begin
         for Worker of Installation.Employees loop
            Worker.Days_Remaining := Worker.Days_Remaining - 1;
            if Worker.Days_Remaining = 0 then
               declare
                  Work : constant Concorde.Commodities.Commodity_Type :=
                           Worker.Pop.Group.Work_Commodity;
               begin
                  Installation.Remove_Quantity
                    (Work, Installation.Get_Quantity (Work));
                  Worker.Pop.Update.Add_Quantity
                    (Work, Worker.Size,
                     WL.Money.Total (Worker.Wage, Worker.Size));
                  Changed := True;
               end;
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

   ------------------------
   -- New_Port_Contracts --
   ------------------------

   procedure New_Port_Contracts
     (Port      : not null access constant Root_Installation_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : WL.Quantities.Quantity_Type;
      Price     : WL.Money.Price_Type)
   is
      use type Concorde.Calendar.Time;
      use WL.Quantities;
      Individual_Contract_Size : constant Quantity_Type :=
                                   To_Quantity (10_000.0);
      Remaining                : Quantity_Type := Quantity;
      Expires                  : constant Concorde.Calendar.Time :=
                                   Concorde.Calendar.Clock
                                     + Concorde.Calendar.Days (7);
   begin
      if Port.Contracted_To_Buy (Commodity) > Zero then
         Port.Update.Delete_Pending_Offers (Commodity);
      end if;

      while Remaining > Zero loop
         declare
            use WL.Money;
            Quantity : constant Quantity_Type :=
                         Min (Remaining, Individual_Contract_Size);
            Value    : constant Money_Type :=
                         Total (Price, Quantity);
         begin
            exit when Value > Port.Limit_Cash;

            declare
               Contract : constant Concorde.Contracts.Contract_Type :=
                            Concorde.Contracts.New_Buy_Contract
                              (Location  => Port.Current_Location,
                               Buyer     => Port,
                               Commodity => Commodity,
                               Quantity  => Quantity,
                               Price     => Price,
                               Expires   => Expires);
            begin
               Port.Update.Add_Contract (Contract);
               Remaining := Remaining - Contract.Quantity;
               Port.Log ("new contract: " & Contract.Show);
            end;
         end;
      end loop;
   end New_Port_Contracts;

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
               declare
                  Commodity : constant Concorde.Commodities.Commodity_Type :=
                                Worker.Pop.Group.Work_Commodity;
               begin
                  Installation.Remove_Quantity
                    (Commodity, Installation.Get_Quantity (Commodity));
                  Worker.Pop.Update.Add_Quantity
                    (Commodity, Worker.Size,
                     WL.Money.Total (Worker.Wage, Worker.Size));
               end;
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

   ----------------------------
   -- Set_Artisan_Production --
   ----------------------------

   procedure Set_Artisan_Production
     (Installation : in out Root_Installation_Type'Class;
      Facility     : Concorde.Facilities.Facility_Type)
   is
   begin
      Installation.Facility := Facility;
   end Set_Artisan_Production;

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
