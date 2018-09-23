with Concorde.Powers.Ministries;

package body Concorde.People.Pops.Work is

   type Pay_Work_Item is
     new Root_Pop_Work_Item with
      record
         Wage : Concorde.Money.Price_Type;
      end record;

   overriding function Show (Item : Pay_Work_Item) return String
   is ("pay wage " & Concorde.Money.Show (Item.Wage) & " to "
       & Concorde.Quantities.Show (Item.Pop.Size_Quantity)
       & " " & Item.Pop.Group.Name & "; total "
       & Concorde.Money.Show
         (Concorde.Money.Total (Item.Wage, Item.Pop.Size_Quantity)));

   overriding function Power
     (Item : Pay_Work_Item)
      return Concorde.Powers.Power_Type
   is (Concorde.Powers.Ministries.Pay (Item.Pop.Group));

   ------------------------
   -- Pay_State_Employee --
   ------------------------

   function Pay_State_Employee
     (Pop  : Concorde.People.Pops.Pop_Type;
      Wage : Concorde.Money.Price_Type)
      return Concorde.Work.Work_Item
   is
   begin
      return new Pay_Work_Item'
        (Concorde.Work.Root_Work_Item with
           Pop  => Pop,
           Wage => Wage);
   end Pay_State_Employee;

end Concorde.People.Pops.Work;
