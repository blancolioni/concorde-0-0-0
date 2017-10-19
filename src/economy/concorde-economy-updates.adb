with Ada.Containers.Doubly_Linked_Lists;

with Memor;

with Concorde.Agents;
with Concorde.Commodities;

with Concorde.Markets;

with Concorde.People.Pops.Consumption;

with Concorde.Installations.Production;

with Concorde.Worlds.Updates;

with WL.Money;
with WL.Quantities;

package body Concorde.Economy.Updates is

   ------------------
   -- Daily_Update --
   ------------------

   procedure Daily_Update is

      package List_Of_Agents is
        new Ada.Containers.Doubly_Linked_Lists
          (Element_Type => Concorde.Agents.Agent_Type,
           "="          => Concorde.Agents."=");

      Delayed_Installation_Trade_Offers : List_Of_Agents.List;

      Virtual : constant Concorde.Commodities.Array_Of_Commodities :=
                  Concorde.Commodities.Get
                    (Concorde.Commodities.Virtual);

      procedure Agent_Trade_Update
        (Agent : not null access Concorde.Agents.Root_Agent_Type'Class);

      ------------------------
      -- Agent_Trade_Update --
      ------------------------

      procedure Agent_Trade_Update
        (Agent : not null access Concorde.Agents.Root_Agent_Type'Class)
      is
      begin
         for Commodity of Virtual loop
            Agent.Set_Quantity (Commodity, Quantities.Zero, Money.Zero);
         end loop;
         Agent.Before_Market;

         if Agent.Delayed_Trade_Offers then
            if Agent.all in
              Concorde.Installations.Root_Installation_Type'Class
            then
               Delayed_Installation_Trade_Offers.Append
                 (Concorde.Agents.Agent_Type (Agent));
            else
               raise Constraint_Error with
                 "cannot delay trade: " & Agent.Short_Name;
            end if;
         else
            Agent.Add_Trade_Offers;
         end if;

      end Agent_Trade_Update;

   begin
      Concorde.Markets.Update_Markets
        (Concorde.Markets.Before_Trading'Access);

      Concorde.Agents.Update_Agents (Agent_Trade_Update'Access);

      for Agent of Delayed_Installation_Trade_Offers loop
         declare
            Installation : constant
              Concorde.Installations.Installation_Type :=
                Concorde.Installations.Installation_Type (Agent);
         begin
            Installation.Add_Trade_Offers;
         end;
      end loop;

      Concorde.Worlds.Scan_Market_Worlds
        (Concorde.Worlds.Updates.Execute_Trades'Access);

      Concorde.Installations.Production.Execute_Production;

      Concorde.People.Pops.Consumption.Execute_Consumption;

   end Daily_Update;

end Concorde.Economy.Updates;
