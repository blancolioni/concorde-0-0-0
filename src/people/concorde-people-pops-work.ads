with Concorde.Money;
with Concorde.Work;

package Concorde.People.Pops.Work is

   function Pay_State_Employee
     (Pop  : Concorde.People.Pops.Pop_Type;
      Wage : Concorde.Money.Price_Type)
      return Concorde.Work.Work_Item;

private

   type Root_Pop_Work_Item is
     abstract new Concorde.Work.Root_Work_Item with
      record
         Pop : Concorde.People.Pops.Pop_Type;
      end record;

end Concorde.People.Pops.Work;
