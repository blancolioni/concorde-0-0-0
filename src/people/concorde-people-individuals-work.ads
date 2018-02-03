with Concorde.Ministries;
with Concorde.Work;

package Concorde.People.Individuals.Work is

   function Appoint_Minister
     (Ministry : not null access constant
        Concorde.Ministries.Root_Ministry_Type'Class)
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
