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

end Concorde.Agents;
