with Concorde.Armies;
with Concorde.Ministries;
with Concorde.People.Pops;
with Concorde.Ships;
with Concorde.Work;

with Concorde.Money;

package Concorde.People.Individuals.Work is

   function Appoint_Minister
     (Ministry : not null access constant
        Concorde.Ministries.Root_Ministry_Type'Class)
      return Concorde.Work.Work_Item;

   function Appoint_General
     (Army : not null access constant
        Concorde.Armies.Root_Army_Type'Class)
      return Concorde.Work.Work_Item;

   function Appoint_Trader_Captain
     (Ship : not null access constant
        Concorde.Ships.Root_Ship_Type'Class)
      return Concorde.Work.Work_Item;

   function Pay_State_Employee
     (Pop  : Concorde.People.Pops.Pop_Type;
      Wage : Concorde.Money.Price_Type)
      return Concorde.Work.Work_Item;

   procedure Perform_Work
     (Individual : Individual_Type;
      Work       : Concorde.Work.Work_Item);

private

   type Root_Individual_Work_Item is
     abstract new Concorde.Work.Root_Work_Item with
      record
         null;
      end record;

   procedure Execute
     (Work       : Root_Individual_Work_Item;
      Individual : Individual_Type)
   is abstract;

end Concorde.People.Individuals.Work;
