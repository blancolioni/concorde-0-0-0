package body Concorde.Agents is

   --------------
   -- Add_Cash --
   --------------

   procedure Add_Cash
     (Agent  : in out Root_Agent_Type'Class;
      Amount : Concorde.Money.Money_Type)
   is
      use type Concorde.Money.Money_Type;
   begin
      Agent.Set_Cash (Agent.Cash + Amount);
   end Add_Cash;

   ----------
   -- Cash --
   ----------

   function Cash
     (Agent : Root_Agent_Type'Class)
      return Concorde.Money.Money_Type
   is
   begin
      return Agent.Cash;
   end Cash;

   ------------------
   -- Get_Quantity --
   ------------------

   overriding function Get_Quantity
     (Agent : Root_Agent_Type;
      Item  : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity
   is
   begin
      return Agent.Stock.Get_Quantity (Item);
   end Get_Quantity;

   -----------------
   -- Remove_Cash --
   -----------------

   procedure Remove_Cash
     (Agent  : in out Root_Agent_Type'Class;
      Amount : Concorde.Money.Money_Type)
   is
      use type Concorde.Money.Money_Type;
   begin
      Agent.Set_Cash (Agent.Cash - Amount);
   end Remove_Cash;

   --------------
   -- Set_Cash --
   --------------

   procedure Set_Cash
     (Agent  : in out Root_Agent_Type'Class;
      Amount : Concorde.Money.Money_Type)
   is
   begin
      Agent.Cash := Amount;
   end Set_Cash;

   ------------------
   -- Set_Quantity --
   ------------------

   overriding procedure Set_Quantity
     (Agent    : in out Root_Agent_Type;
      Item     : Concorde.Commodities.Commodity_Type;
      Quantity : Concorde.Quantities.Quantity)
   is
   begin
      Agent.Stock.Set_Quantity (Item, Quantity);
   end Set_Quantity;

end Concorde.Agents;
