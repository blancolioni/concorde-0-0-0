with Concorde.Real_Images;

package body Concorde.Industries is

   ------------------
   -- Daily_Budget --
   ------------------

   overriding function Daily_Budget
     (Industry  : Root_Industry_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Unit_Real
   is
   begin
      return Industry.Production.Relative_Input_Cost (Commodity);
   end Daily_Budget;

   -----------------
   -- Daily_Needs --
   -----------------

   overriding function Daily_Needs
     (Industry  : Root_Industry_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Non_Negative_Real
   is
   begin
      return Concorde.Quantities.To_Real
        (Industry.Production.Input_Quantity
           (Commodity => Commodity,
            Size      => Industry.Production_Size));
   end Daily_Needs;

   ------------------
   -- Daily_Supply --
   ------------------

   overriding function Daily_Supply
     (Industry  : Root_Industry_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Non_Negative_Real
   is
   begin
      if Industry.Production.Is_Output (Commodity) then
         return Concorde.Quantities.To_Real
           (Industry.Get_Quantity (Commodity));
      else
         return 0.0;
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

      if Industry.Production_Count > 1 then
         Max_Size :=
           Real'Max
             (Real'Min
                (Industry.Production.Minimum_Size (Industry.Sold),
                 Industry.Size),
              Industry.Size / 4.0);

         declare
            use Concorde.Money;
            Earn : Money_Type := Zero;
         begin
            for Commodity of Industry.Production.Outputs loop
               Earn := Earn + Industry.Sold.Get_Value (Commodity);
            end loop;

            Industry.Log ("last time earned "
                          & Show (Earn)
                          & " after spending "
                          & Show (Industry.Cost));

            if Earn < Adjust (Industry.Cost, 1.01) then
               if Max_Size > Industry.Production_Size then
                  Max_Size := Industry.Production_Size;
               end if;
               Max_Size := Max_Size * 0.9;
            end if;
         end;

      end if;

      Industry.Production.Execute
        (Stock => Industry,
         Size  => Industry.Production_Size,
         Cost  => Industry.Cost);

      Industry.Production_Size := Max_Size;
      Industry.Sold.Clear_Stock;
      Industry.Production_Count := Industry.Production_Count + 1;

      Industry.Log ("next production size: "
                    & Concorde.Real_Images.Approximate_Image
                      (Industry.Production_Size));
   end Execute_Production;

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
