with Concorde.People.Individuals;

package body Concorde.Bureaucracy is

   -------------------
   -- Add_Work_Item --
   -------------------

   procedure Add_Work_Item
     (Bureaucracy : in out Bureaucratic_Interface'Class;
      Work        : Concorde.Work.Work_Item)
   is
   begin
      Bureaucracy.Director.Manager.Add_Work_Item (Work);
   end Add_Work_Item;

   ---------------------
   -- Find_With_Power --
   ---------------------

   function Find_With_Power
     (Item : not null access constant Bureaucratic_Interface'Class;
      Power : Concorde.Powers.Power_Type)
      return not null access constant Bureaucratic_Interface'Class
   is
   begin
      if Item.Has_Power (Power) then
         return Item;
      elsif Item.Has_Delegated_Power (Power) then
         return Item.Delegated_To (Power).Find_With_Power (Power);
      else
         raise Constraint_Error with
           "expected to find a bureaucracy with power "
           & Concorde.Powers.Show (Power);
      end if;
   end Find_With_Power;

end Concorde.Bureaucracy;
