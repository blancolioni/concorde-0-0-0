with WL.Heaps;
with WL.Money;

with Concorde.Commodities.Needs;
with Concorde.Worlds;

package body Concorde.People.Pops is

   type Artisan_Consumption is
      record
         Commodity : Concorde.Commodities.Commodity_Type;
         Cost_Per  : WL.Money.Money_Type;
         Input_Per : WL.Quantities.Quantity_Type;
         Have      : WL.Quantities.Quantity_Type;
      end record;

   package Consumption_Queues is
     new WL.Heaps (WL.Money.Money_Type, Artisan_Consumption,
                   WL.Money.">");

   ----------------------
   -- Add_Trade_Offers --
   ----------------------

   procedure Add_Trade_Offers
     (Pop : not null access constant Root_Pop_Type)
   is

      use WL.Money, WL.Quantities;
      use Concorde.Commodities;

      Remaining_Budget : Money_Type := Pop.Cash;
      Artisan_Budget   : Money_Type := Zero;

      procedure Add_Bid
        (Commodity : Concorde.Commodities.Commodity_Type;
         Quantity  : WL.Quantities.Quantity_Type;
         Price     : WL.Money.Price_Type);

      procedure Add_Sell_Offer
        (Commodity : Concorde.Commodities.Commodity_Type);

      procedure Check_Artisan_Inputs;

      procedure Check_Need
        (Level : Concorde.People.Groups.Need_Level);

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

      --------------------
      -- Add_Sell_Offer --
      --------------------

      procedure Add_Sell_Offer
        (Commodity : Concorde.Commodities.Commodity_Type)
      is
      begin
         if Pop.Current_Ask_Quantity (Commodity)
           < Pop.Get_Quantity (Commodity)
         then
            Pop.Create_Ask
              (Commodity,
               Pop.Get_Quantity (Commodity)
               - Pop.Current_Ask_Quantity (Commodity));
         end if;
      end Add_Sell_Offer;

      --------------------------
      -- Check_Artisan_Inputs --
      --------------------------

      procedure Check_Artisan_Inputs is
         Facility : constant Concorde.Facilities.Facility_Type :=
                      Pop.Production;
         Need     : Concorde.Commodities.Needs.Commodity_Needs;
      begin

         Concorde.Commodities.Needs.Set_Budget (Need, Artisan_Budget);

         for I in 1 .. Facility.Input_Count loop
            if Facility.Simple_Input (I) then
               declare
                  Commodity : constant Concorde.Commodities.Commodity_Type :=
                                Facility.Input_Commodity (I);
                  Required  : constant Quantity_Type :=
                                Facility.Input_Quantity
                                  (Pop.Size_Quantity, I);
                  Have      : constant Quantity_Type :=
                                Pop.Get_Quantity (Commodity)
                                + Pop.Current_Bid_Quantity (Commodity);
               begin
                  if Required > Have then
                     Concorde.Commodities.Needs.Add_Need
                       (Need      => Need,
                        Commodity => Commodity,
                        Quantity  => Required - Have,
                        Price     => Pop.Create_Bid_Price (Commodity));
                  end if;
               end;
            elsif Facility.Choice_Input (I) then
               declare
                  Cheapest_Item  : Concorde.Commodities.Commodity_Type;
                  Cheapest_Price : Price_Type := Zero;
                  Cheapest_Cost  : Money_Type := Zero;
                  Cheapest_Reqd  : Quantity_Type := Zero;
               begin
                  for J in 1 .. Facility.Input_Choice_Count (I) loop
                     declare
                        Input : constant Concorde.Commodities.Commodity_Type :=
                                  Facility.Input_Choice_Commodity (I, J);
                        Need  : constant Quantity_Type :=
                                  Facility.Input_Choice_Quantity
                                    (Pop.Size_Quantity, I, J);
                        Have  : constant Quantity_Type :=
                                  Pop.Get_Quantity (Input)
                                  + Pop.Current_Bid_Quantity (Input);
                        Reqd  : constant Quantity_Type :=
                                  (if Need < Have then Zero
                                   else Need - Have);
                        Price : constant Price_Type :=
                                  Pop.Mean_Price_Belief (Input);
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
                     Concorde.Commodities.Needs.Add_Need
                       (Need      => Need,
                        Commodity => Cheapest_Item,
                        Quantity  => Cheapest_Reqd,
                        Price     => Pop.Create_Bid_Price (Cheapest_Item));
                  end if;
               end;
            else
               raise Constraint_Error with
               Pop.Short_Name
                 & "cannot understand input rules for input"
                 & I'Img;
            end if;
         end loop;

         Concorde.Commodities.Needs.Scan_Needs
           (Need, Add_Bid'Access);

      end Check_Artisan_Inputs;

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

      if Pop.Group.Is_Artisan then
         Remaining_Budget := Adjust (Remaining_Budget, 0.5);
         Artisan_Budget := Pop.Cash - Remaining_Budget;
      end if;

      for Level in Concorde.People.Groups.Need_Level loop
         Check_Need (Level);
         exit when Remaining_Budget <= Zero;
      end loop;

      if Pop.Group.Is_Artisan
        and then Pop.Has_Production
      then
         Check_Artisan_Inputs;
      end if;

      if Pop.Group.Unemployment
        and then Pop.Get_Quantity (Pop.Group.Work_Commodity) > Zero
      then
         Pop.Create_Ask
           (Commodity    => Pop.Group.Work_Commodity,
            Ask_Quantity => Pop.Get_Quantity (Pop.Group.Work_Commodity));
      end if;

      if Pop.Group.Is_Artisan
        and then Pop.Has_Production
      then
         Pop.Update.Execute_Production;
         Add_Sell_Offer (Pop.Production.Output);
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

      -------------
      -- Consume --
      -------------

      procedure Consume
        (Commodity : Concorde.Commodities.Commodity_Type;
         Required  : WL.Quantities.Quantity_Type)
      is
         Consumed : constant Quantity_Type :=
                      Min (Required, Pop.Get_Quantity (Commodity));
      begin
         Pop.Log ("consumption",
                  Commodity.Name & ": required "
                  & Show (Required) & ", consumed "
                  & Show (Consumed));
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

   ------------------------
   -- Execute_Production --
   ------------------------

   procedure Execute_Production
     (Pop : in out Root_Pop_Type'Class)
   is
      use WL.Quantities;
      use Concorde.Commodities;
      Facility        : constant Concorde.Facilities.Facility_Type :=
                          Pop.Production;
      Production_Cost : WL.Money.Money_Type :=
                          WL.Money.Zero;

      Production_Size : constant WL.Quantities.Quantity_Type :=
                          Pop.Size_Quantity;

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
                               (Pop.Get_Average_Price (Commodity),
                                Quantity);
            begin
               Queue.Insert (Cost_Per,
                             Artisan_Consumption'
                               (Commodity => Commodity,
                                Cost_Per  => Cost_Per,
                                Input_Per => Quantity,
                                Have      =>
                                  Pop.Get_Quantity (Commodity)));
            end;
         end loop;

         declare
            Remaining : Unit_Real := Throughput;
         begin
            while not Queue.Is_Empty and then Remaining > 0.0 loop
               declare
                  Item     : constant Artisan_Consumption :=
                               Queue.Maximum_Element;
                  Required : constant Quantity_Type :=
                               Scale (Item.Input_Per, Float (Remaining));
                  Cost     : Money_Type;
               begin
                  Queue.Delete_Maximum;

                  if Required <= Item.Have then
                     Remaining := 0.0;
                     Cost :=
                       Total (Pop.Get_Average_Price (Item.Commodity),
                              Required);
                     Pop.Remove_Quantity
                       (Item.Commodity, Required);
                  else
                     Cost := Pop.Get_Value (Item.Commodity);
                     Remaining :=
                       Real'Max
                         (0.0,
                          Remaining
                          - Unit_Real (To_Float (Item.Have)
                            / To_Float (Required)));
                     Pop.Remove_Quantity
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
                               (Pop.Size_Quantity, Input_Index, I);
               Available : constant Quantity_Type :=
                             Pop.Get_Quantity (Commodity);
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

      Base_Production    : constant Quantity_Type :=
                             Facility.Base_Output_Quantity
                               (Pop.Size_Quantity, 1.0);
      Effective_Capacity : Quantity_Type := Base_Production;
      Input_Limit        : Unit_Real := 1.0;

   begin

      Pop.Log_Production
        ("size: " & Show (Pop.Size_Quantity)
         & "; base production: "
         & Show (Base_Production));

      if Facility.Has_Output
        and then Facility.Output.Is_Set (Virtual)
      then
         Pop.Log_Production ("clearing virtual commodity "
                             & Facility.Output.Name);
         Pop.Set_Quantity
           (Facility.Output, WL.Quantities.Zero, WL.Money.Zero);
      end if;

      for Input_Index in 1 .. Facility.Input_Count loop
         declare
            T : constant Unit_Real :=
                  Get_Input_Throughput (Input_Index);
         begin
            Input_Limit := Non_Negative_Real'Min (Input_Limit, T);
         end;
      end loop;

      if Input_Limit > 0.0 then
         for Input_Index in 1 .. Facility.Input_Count loop
            Consume_Input
              (Input_Index, Input_Limit);
         end loop;

         Effective_Capacity :=
           Scale (Effective_Capacity, Float (Input_Limit));

         if Effective_Capacity > Pop.Available_Capacity then
            Pop.Log_Production
              ("production reduced from " & Show (Effective_Capacity)
               & " to " & Show (Pop.Available_Capacity)
               & " because of available capacity");

            Effective_Capacity := Pop.Available_Capacity;
         end if;

         if Effective_Capacity > Zero then
            Pop.Add_Quantity
              (Facility.Output,
               Effective_Capacity,
               Production_Cost);

            Pop.Log_Production
              ("produces "
               & Image (Effective_Capacity)
               & " "
               & Facility.Output.Name
               & " for "
               & Image (Production_Cost)
               & "; stock now "
               & Image (Pop.Get_Quantity (Facility.Output)));

         end if;
      end if;

   end Execute_Production;

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

   --------------------
   -- Set_Production --
   --------------------

   procedure Set_Production
     (Pop        : in out Root_Pop_Type'Class;
      Production : Concorde.Facilities.Facility_Type)
   is
   begin
      Pop.Production_Facility := Production;
      Pop.Production_Started := Concorde.Calendar.Clock;
   end Set_Production;

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
