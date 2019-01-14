with Concorde.Real_Images;

with Concorde.People.Communities;
with Concorde.Worlds;

package body Concorde.Industries is

   -------------------
   -- Create_Budget --
   -------------------

   procedure Create_Budget
     (Industry : in out Root_Industry_Type'Class)
   is
      use Concorde.Commodities;
      use Concorde.Money, Concorde.Quantities;

      Required          : Concorde.Commodities.Virtual_Stock_Type;
      Capital_Commodity : Commodity_Type := null;
      Capital_Quantity  : Quantity_Type  := Zero;

      procedure Set_Cost (Commodity : Concorde.Commodities.Commodity_Type);

      --------------
      -- Set_Cost --
      --------------

      procedure Set_Cost (Commodity : Concorde.Commodities.Commodity_Type) is
         Quantity  : Quantity_Type := Required.Get_Quantity (Commodity);
         Value     : Money_Type;
         Available : constant Quantity_Type :=
                       Industry.Get_Quantity (Commodity);
      begin
         if Commodity.Is_Pop_Group
           or else Commodity.Is_Set (Concorde.Commodities.Transient)
         then
            null;
         elsif Quantity <= Available then
            Quantity := Zero;
         else
            Quantity := Quantity - Available;
         end if;

         Value :=
           Total
             (Industry.Community.Market.Current_Price (Commodity),
              Quantity);

         Industry.Log (Commodity.Name
                       & ": required "
                       & Show (Required.Get_Quantity (Commodity))
                       & "; missing "
                       & Show (Quantity)
                       & "; cost "
                       & Show (Value));

         Required.Set_Quantity
           (Item     => Commodity,
            Quantity => Quantity,
            Value    => Value);

         if Quantity > Zero
           and then Industry.Get_Quantity (Commodity) > Zero
           and then Industry.Production.Input_Consumption (Commodity) < 0.5
         then
            if Capital_Commodity = null
              or else Industry.Get_Quantity (Commodity) > Capital_Quantity
            then
               Capital_Commodity := Commodity;
               Capital_Quantity := Industry.Get_Quantity (Commodity);
            end if;
         end if;

      end Set_Cost;

   begin
      Industry.Production.Calculate_Input_Requirements
        (Size        => Industry.Production_Size,
         Consumption => Required);
      Industry.Budget.Clear_Stock;
      Required.Scan_Stock (Set_Cost'Access);

      declare
         Proposed_Budget  : constant Money_Type :=
                              Required.Total_Virtual_Value;
         Limit_Cash       : constant Money_Type := Industry.Limit_Cash;
         Available_Budget : constant Money_Type :=
                              Min (Adjust (Industry.Cash, 1.5),
                                   Industry.Limit_Cash);
         New_Size         : Non_Negative_Real := Industry.Production_Size;
         Missing          : Commodity_Type := null;
         Missing_Quantity : Quantity_Type;
      begin
         Industry.Log ("initial proposed budget: "
                       & Show (Proposed_Budget)
                       & "; cash " & Show (Available_Budget)
                       & "; limit " & Show (Limit_Cash)
                       & "; capital commodity: "
                       & (if Capital_Commodity = null then "none"
                         else Show (Capital_Quantity) & " x "
                         & Capital_Commodity.Name));

         if Proposed_Budget > Available_Budget then
            if Capital_Commodity /= null then
               --  drop capital investment

               New_Size := New_Size
                 * To_Real (Industry.Get_Quantity (Capital_Commodity))
                 / To_Real (Industry.Production.Input_Requirement
                            (Capital_Commodity, Industry.Production_Size));
               Industry.Production.Calculate_Input_Requirements
                 (Size        => New_Size,
                  Consumption => Required);
               Missing := Capital_Commodity;
               Missing_Quantity := Capital_Quantity;
               Capital_Commodity := null;
               Required.Scan_Stock (Set_Cost'Access);

               --  fixme: use (some of) remaining budget for
               --  capital investment, maybe if the budget
               --  provides more production than the existing
               --  capital commodity can support

               Industry.Log ("revised budget: "
                             & Show (Required.Total_Virtual_Value)
                             & "; cash " & Show (Available_Budget)
                             & "; limit " & Show (Limit_Cash)
                             & "; capital commodity: "
                             & (if Capital_Commodity = null then "none"
                               else Show (Capital_Quantity) & " x "
                               & Capital_Commodity.Name));
            end if;
         end if;

         declare
            Adjusted_Budget : constant Money_Type :=
                                Required.Total_Virtual_Value;
            Adjustment      : constant Non_Negative_Real :=
                                Real'Min
                                  (To_Real (Available_Budget)
                                   / To_Real (Adjusted_Budget),
                                   1.5);

            procedure Set_Budget (Commodity : Commodity_Type);

            ----------------
            -- Set_Budget --
            ----------------

            procedure Set_Budget (Commodity : Commodity_Type) is
            begin
               Industry.Budget.Set_Quantity
                 (Commodity,
                  Required.Get_Quantity (Commodity),
                  Adjust (Required.Get_Value (Commodity), Adjustment));
               Industry.Log (Commodity.Name
                             & ": want "
                             & Show (Required.Get_Quantity (Commodity))
                             & " proposed budget "
                             & Show (Required.Get_Value (Commodity))
                             & " final budget "
                             & Show
                               (Industry.Budget.Get_Value
                                  (Commodity)));
               if Industry.Budget.Get_Quantity (Commodity) = Zero then
                  Industry.Create_Ask (Commodity, Zero);
               end if;

            end Set_Budget;

         begin
            Required.Scan_Stock (Set_Budget'Access);

            if Missing /= null
              and then Industry.Budget.Total_Virtual_Value
                < Available_Budget
            then
               declare
                  Remaining : constant Money_Type :=
                                Available_Budget
                                  - Industry.Budget.Total_Virtual_Value;
                  Price     : constant Price_Type :=
                                Industry.Community.Market.Current_Price
                                  (Missing);
                  Quantity  : constant Quantity_Type :=
                                Min (Missing_Quantity,
                                     Get_Quantity (Remaining, Price));
               begin
                  Industry.Budget.Set_Quantity
                    (Missing, Quantity,
                     Total (Price, Quantity));
                  Industry.Log
                    (Missing.Name
                     & ": capital investment: want "
                     & Show (Quantity)
                     & " budget "
                     & Show
                       (Industry.Budget.Get_Value
                            (Missing))
                     & " price "
                     & Show (Price));
               end;
            end if;

         end;
      end;

      declare
         procedure Create_Bid
           (Commodity : Concorde.Commodities.Commodity_Type);

         ----------------
         -- Create_Bid --
         ----------------

         procedure Create_Bid
           (Commodity : Concorde.Commodities.Commodity_Type)
         is
            Bid_Price : constant Concorde.Money.Price_Type :=
                          Industry.Create_Bid_Price (Commodity);
         begin
            Industry.Create_Bid
              (Commodity,
               Concorde.Quantities.Min
                 (Industry.Budget.Get_Quantity (Commodity),
                  Concorde.Money.Get_Quantity (Industry.Cash, Bid_Price)));
         end Create_Bid;

      begin
         Industry.Budget.Scan_Stock (Create_Bid'Access);
      end;

      Industry.Supply.Clear_Stock;
      for Commodity of Industry.Production.Outputs loop
         Industry.Create_Ask
           (Commodity, Industry.Get_Quantity (Commodity));
      end loop;

   end Create_Budget;

   ------------------
   -- Daily_Budget --
   ------------------

   overriding function Daily_Budget
     (Industry  : Root_Industry_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Money_Type
   is
      use type Concorde.Money.Money_Type;
   begin
      return Industry.Budget.Get_Value (Commodity)
        + Industry.Supply.Get_Value (Commodity);
   end Daily_Budget;

   -----------------
   -- Daily_Needs --
   -----------------

   overriding function Daily_Needs
     (Industry  : Root_Industry_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
   is
      use type Concorde.Quantities.Quantity_Type;
      Budget : constant Quantities.Quantity_Type :=
                 Industry.Budget.Get_Quantity (Commodity);
   begin
      if Budget > Quantities.Zero then
         return Industry.Get_Quantity (Commodity)
           + Industry.Budget.Get_Quantity (Commodity);
      else
         return Quantities.Zero;
      end if;
   end Daily_Needs;

   ------------------
   -- Daily_Supply --
   ------------------

   overriding function Daily_Supply
     (Industry  : Root_Industry_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Industry.Supply.Get_Quantity (Commodity);
   end Daily_Supply;

   ------------------------
   -- Execute_Production --
   ------------------------

   procedure Execute_Production
     (Industry : in out Root_Industry_Type'Class)
   is
      Max_Size  : Non_Negative_Real := Industry.Size;
   begin
      Industry.Log ("executing production");

      declare
         use Concorde.Money, Concorde.Quantities;
         Earn    : Money_Type    := Zero;
         Sold    : Quantity_Type := Zero;
         Stock   : Quantity_Type := Zero;
         History : Quantity_Type := Zero;
      begin
         for Commodity of Industry.Production.Outputs loop
            Earn := Earn + Industry.Sold.Get_Value (Commodity);
            Sold := Sold + Industry.Sold.Get_Quantity (Commodity);
            Stock := Stock + Industry.Get_Quantity (Commodity);

            declare
               Current_Sales : constant Quantity_Type :=
                                 Industry.Historical_Sales.Get_Quantity
                                   (Commodity);
               New_Sales     : constant Quantity_Type :=
                                 Scale (Current_Sales, 0.5)
                                 + Scale (Sold, 0.5);
               Current_Value : constant Money_Type :=
                                 Industry.Historical_Sales.Get_Value
                                   (Commodity);
               New_Value     : constant Money_Type :=
                                 Adjust (Current_Value, 0.5)
                                 + Adjust (Earn, 0.5);
            begin
               if Industry.Production_Count <= 2
                 or else Current_Sales = Zero
               then
                  Industry.Historical_Sales.Set_Quantity
                    (Commodity, Sold, Earn);
               else
                  Industry.Historical_Sales.Set_Quantity
                    (Commodity, New_Sales, New_Value);
                  History := History + New_Sales;
               end if;
            end;
         end loop;

         if Industry.Production_Count > 2 then
            Industry.Log ("last time earned "
                          & Show (Earn)
                          & " selling " & Show (Sold)
                          & " units after spending "
                          & Show (Industry.Cost)
                          & "; there are "
                          & Show (Stock) & " units remaining");

            if Industry.Cost > Zero then
               if Earn < Adjust (Industry.Cost, 1.01) then
                  if Max_Size > Industry.Production_Size then
                     Max_Size := Industry.Production_Size;
                  end if;
                  Max_Size := Max_Size * 0.9;
                  Industry.Log ("poor earnings ("
                                & Show (Earn)
                                & ") reduce max production to "
                                & Concorde.Real_Images.Approximate_Image
                                  (Max_Size));
               end if;

               if Stock > Scale (Sold, 0.1) then
                  declare
                     Demand_Size : constant Non_Negative_Real :=
                                     Industry.Production.Minimum_Size
                                       (Industry.Historical_Sales);
                  begin
                     Max_Size := Real'Min (Max_Size, Demand_Size * 1.1);
                     Max_Size :=
                       Real'Max (Max_Size, Industry.Production_Size / 4.0);
                     Industry.Log ("poor sales ("
                                   & Show (History)
                                   & ") reduce max production to "
                                   & Concorde.Real_Images.Approximate_Image
                                     (Max_Size));
                  end;
               end if;

               if Stock > Scale (Sold, 0.8) then
                  Max_Size := Max_Size / 2.0;
                  Industry.Log ("excess stock ("
                                & Show (Stock)
                                & ") reduce max production to "
                                & Concorde.Real_Images.Approximate_Image
                                  (Max_Size));
               end if;
            end if;
         end if;

      end;

      Industry.Log ("this production size: "
                    & Concorde.Real_Images.Approximate_Image
                      (Industry.Production_Size));

      Industry.Production.Execute
        (Producer    => Db.Reference (Industry.Reference),
         Environment => Industry.Community.World.Update,
         Stock       => Industry,
         Size        => Industry.Production_Size,
         Limit_Items => Industry.Limit_Items,
         Cost        => Industry.Cost);

      if not Industry.Limit_Items.Is_Empty then
         for Item of Industry.Limit_Items loop
            Industry.Log ("production limited by " & Item.Name);
         end loop;
      end if;

      for Commodity of Concorde.Commodities.Get
        (Concorde.Commodities.Pop_Group)
      loop
         Industry.Set_Quantity (Commodity, Concorde.Quantities.Zero,
                                Concorde.Money.Zero);
      end loop;

      Industry.Production_Size := Max_Size;
      Industry.Sold.Clear_Stock;
      Industry.Production_Count := Industry.Production_Count + 1;

   end Execute_Production;

   ----------------
   -- Identifier --
   ----------------

   overriding function Identifier
     (Industry : Root_Industry_Type) return String
   is
   begin
      return Memor.To_String (Industry.Reference)
        & "-" & Industry.Production.Identifier;
   end Identifier;

   -----------------------
   -- On_Commodity_Sell --
   -----------------------

   overriding procedure On_Commodity_Sell
     (Industry  : in out Root_Industry_Type;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type)
   is
   begin
      Concorde.Agents.Root_Agent_Type (Industry)
        .On_Commodity_Sell (Commodity, Quantity, Price);
      Industry.Sold.Add_Quantity
        (Commodity, Quantity, Concorde.Money.Total (Price, Quantity));
   end On_Commodity_Sell;

   -----------------
   -- Set_Manager --
   -----------------

   overriding procedure Set_Manager
     (Industry    : in out Root_Industry_Type;
      Manager     : Concorde.Managers.Manager_Type)
   is
   begin
      Industry.Manager := Manager;
   end Set_Manager;

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant Root_Industry_Type'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update;

   ------------------
   -- Update_Agent --
   ------------------

   overriding procedure Update_Agent
     (Industry            : not null access constant Root_Industry_Type;
      Perform_Update : not null access
        procedure (Agent : in out Concorde.Agents.Root_Agent_Type'Class))
   is
   begin
      Perform_Update (Industry.Update);
   end Update_Agent;
end Concorde.Industries;
