with Concorde.Real_Images;

with Concorde.People.Communities;
with Concorde.Worlds;

package body Concorde.Industries is

   ------------------
   -- Daily_Budget --
   ------------------

   overriding function Daily_Budget
     (Industry  : Root_Industry_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Money_Type
   is
      Proportion : constant Unit_Real :=
                     Industry.Production.Relative_Input_Cost (Commodity);
   begin
      if Proportion > 0.0 then
         Industry.Log ("cash: " & Concorde.Money.Show (Industry.Cash)
                       & "; input: " & Commodity.Identifier
                       & "; proportion: "
                       & Concorde.Real_Images.Approximate_Image (Proportion)
                       & "; budget: "
                       & Concorde.Money.Show
                         (Concorde.Money.Adjust (Industry.Cash, Proportion)));
         return Concorde.Money.Adjust
           (Industry.Cash, Proportion);
      else
         return Concorde.Money.Zero;
      end if;
   end Daily_Budget;

   -----------------
   -- Daily_Needs --
   -----------------

   overriding function Daily_Needs
     (Industry  : Root_Industry_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Industry.Production.Input_Quantity
        (Commodity => Commodity,
         Size      => Industry.Production_Size);
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
      if Industry.Production.Is_Output (Commodity) then
         declare
            use Concorde.Money, Concorde.Quantities;
            Quantity      : constant Quantity_Type :=
                              Industry.Get_Quantity (Commodity);
            Value         : constant Money_Type :=
                              Industry.Get_Value (Commodity);
            This_Price    : constant Price_Type :=
                              Price (Value, Quantity);
            Current_Price : constant Price_Type :=
                              Industry.Community.Current_Price (Commodity);
            Current_Demand : constant Quantity_Type :=
                               Industry.Community.Current_Demand
                                 (Commodity);
            Minimum_Price : constant Price_Type :=
                              Adjust_Price (This_Price, 1.1);
            Factor        : Unit_Real := 1.0;
         begin
            if Minimum_Price > Zero
              and then Current_Price < Minimum_Price
            then
               Factor := To_Real (Current_Price) / To_Real (Minimum_Price);
            end if;

            Industry.Log
              (Commodity.Identifier
               & ": value " & Show (Value)
               & "; minimum price " & Show (Minimum_Price)
               & "; current price " & Show (Current_Price)
               & "; demand " & Show (Current_Demand)
               & "; factor " & Concorde.Real_Images.Approximate_Image (Factor)
               & "; available " & Show (Quantity)
               & "; supply "
               & Show (Min (Current_Demand, Scale (Quantity, Factor))));

            return Min (Current_Demand, Scale (Quantity, Factor));
         end;
      else
         return Concorde.Quantities.Zero;
      end if;
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

      if Industry.Production_Count > 2 then
         declare
            Demand : Concorde.Commodities.Root_Stock_Type;
         begin
            Demand.Create_Stock (Concorde.Quantities.Zero, Virtual => True);
            for Commodity of Industry.Production.Outputs loop
               Demand.Add_Quantity
                 (Commodity, Industry.Community.Current_Demand (Commodity),
                  Concorde.Money.Total
                    (Industry.Community.Current_Price (Commodity),
                     Industry.Community.Current_Demand (Commodity)));
            end loop;

            Max_Size :=
              Real'Max
                (Real'Min
                   (Industry.Production.Minimum_Size (Demand),
                    Industry.Size),
                 Industry.Size / 4.0);
         end;

         declare
            use Concorde.Money, Concorde.Quantities;
            Earn  : Money_Type    := Zero;
            Sold  : Quantity_Type := Zero;
            Stock : Quantity_Type := Zero;
         begin
            for Commodity of Industry.Production.Outputs loop
               Earn := Earn + Industry.Sold.Get_Value (Commodity);
               Sold := Sold + Industry.Sold.Get_Quantity (Commodity);
               Stock := Stock + Industry.Get_Quantity (Commodity);
            end loop;

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
               end if;

               if Stock > Scale (Sold, 0.8) then
                  Max_Size := Max_Size / 2.0;
               end if;
            end if;

         end;

      end if;

      Industry.Production.Execute
        (Environment => Industry.Community.World.Update,
         Stock       => Industry,
         Size        => Industry.Production_Size,
         Cost        => Industry.Cost);

      for Commodity of Concorde.Commodities.Get
        (Concorde.Commodities.Pop_Group)
      loop
         Industry.Set_Quantity (Commodity, Concorde.Quantities.Zero,
                                Concorde.Money.Zero);
      end loop;

      Industry.Production_Size := Max_Size;
      Industry.Sold.Clear_Stock;
      Industry.Production_Count := Industry.Production_Count + 1;

      Industry.Log ("next production size: "
                    & Concorde.Real_Images.Approximate_Image
                      (Industry.Production_Size));
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
