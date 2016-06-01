with Concorde.Commodities;
with Concorde.Money;

package body Concorde.People.Pops.Consumption is

   -------------------------
   -- Execute_Consumption --
   -------------------------

   procedure Execute_Consumption
     (Pop : in out Root_Pop_Type'Class)
   is
      use Concorde.Commodities, Concorde.Quantities;
      Group : constant Concorde.People.Groups.Pop_Group :=
                Pop.Wealth_Group;
      Quality : constant Commodity_Quality := Group.Preferred_Quality;
      Needs : constant Array_Of_Commodities :=
                Concorde.Commodities.Get
                  (Consumer, Quality);
      Minimum : constant Concorde.Quantities.Quantity :=
                  Pop.Size_Quantity;
   begin
      for Need of Needs loop
         declare
            Available : constant Quantity :=
                          Pop.Get_Quantity (Need);
         begin
            if Available >= Minimum then
               Pop.Remove_Quantity (Need, Minimum);
            else
               Pop.Set_Quantity (Need, Zero, Concorde.Money.Zero);
            end if;
         end;
      end loop;
   end Execute_Consumption;

end Concorde.People.Pops.Consumption;
