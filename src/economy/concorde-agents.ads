with Concorde.Money;
with Concorde.Objects;

package Concorde.Agents is

   type Root_Agent_Type is
     abstract new Concorde.Objects.Root_Object_Type with private;

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
     abstract new Concorde.Objects.Root_Object_Type with
      record
         Cash : Concorde.Money.Money_Type;
      end record;

end Concorde.Agents;
