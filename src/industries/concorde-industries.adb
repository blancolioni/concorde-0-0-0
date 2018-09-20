with Concorde.Quantities;

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
            Size      => Industry.Size));
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
   begin
      Industry.Production.Execute
        (Stock => Industry,
         Size  => Industry.Size,
         Cost  => Industry.Cost);
   end Execute_Production;

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
