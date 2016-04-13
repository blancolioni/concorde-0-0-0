with Concorde.Money;
with Concorde.Quantities;

with Concorde.Objects;

with Concorde.Commodities;

package Concorde.Agents is

   type Root_Agent_Type is
     abstract new Concorde.Objects.Root_Object_Type
   and Concorde.Commodities.Stock_Interface with private;

   overriding function Get_Quantity
     (Agent : Root_Agent_Type;
      Item  : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity;

   overriding procedure Set_Quantity
     (Agent    : in out Root_Agent_Type;
      Item     : Concorde.Commodities.Commodity_Type;
      Quantity : Concorde.Quantities.Quantity);

   function Cash
     (Agent : Root_Agent_Type'Class)
      return Concorde.Money.Money_Type;

   procedure Set_Cash
     (Agent  : in out Root_Agent_Type'Class;
      Amount : Concorde.Money.Money_Type);

   procedure Add_Cash
     (Agent : in out Root_Agent_Type'Class;
      Amount : Concorde.Money.Money_Type);

   procedure Remove_Cash
     (Agent  : in out Root_Agent_Type'Class;
      Amount : Concorde.Money.Money_Type);

private

   type Root_Agent_Type is
     abstract new Concorde.Objects.Root_Object_Type
     and Concorde.Commodities.Stock_Interface with
      record
         Stock : Concorde.Commodities.Root_Stock_Type;
         Cash  : Concorde.Money.Money_Type;
      end record;

end Concorde.Agents;
