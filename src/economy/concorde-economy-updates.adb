with Ada.Containers.Doubly_Linked_Lists;

with Memor;

with Concorde.Agents;
with Concorde.Commodities;

with Concorde.People.Pops.Db;
with Concorde.People.Pops.Consumption;

with Concorde.Installations.Db;
with Concorde.Installations.Production;

with Concorde.Ships.Trading;

with Concorde.Worlds.Updates;

with Concorde.Markets.Db;
with Concorde.Ships.Db;
with Concorde.Worlds.Db;

with Concorde.Money;
with Concorde.Quantities;

package body Concorde.Economy.Updates is

   ------------------
   -- Daily_Update --
   ------------------

   procedure Daily_Update is

      package List_Of_References is
        new Ada.Containers.Doubly_Linked_Lists
          (Element_Type => Memor.Database_Reference,
           "="          => Memor."=");

      Delayed_Installation_Trade_Offers : List_Of_References.List;

      Virtual : constant Concorde.Commodities.Array_Of_Commodities :=
                  Concorde.Commodities.Get
                    (Concorde.Commodities.Virtual);

      procedure Agent_Trade_Update
        (Rec : not null access Memor.Root_Record_Type'Class);

      ------------------------
      -- Agent_Trade_Update --
      ------------------------

      procedure Agent_Trade_Update
        (Rec : not null access Memor.Root_Record_Type'Class)
      is
         Agent : constant Concorde.Agents.Agent_Type :=
                   Concorde.Agents.Agent_Type (Rec);
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
                 (Agent.Reference);
            else
               raise Constraint_Error with
                 "cannot delay trade: " & Agent.Short_Name;
            end if;
         else
            Agent.Add_Trade_Offers;
         end if;

      end Agent_Trade_Update;

   begin
      Concorde.Markets.Db.Iterate
        (Concorde.Markets.Before_Trading'Access);

      Concorde.People.Pops.Db.Iterate
        (Agent_Trade_Update'Access);

      Concorde.Installations.Db.Iterate
        (Agent_Trade_Update'Access);

      Concorde.Ships.Db.Iterate
        (Concorde.Ships.Trading.Trade'Access);

      for Reference of Delayed_Installation_Trade_Offers loop
         declare
            Installation : constant Concorde.Installations.Installation_Type :=
                             Concorde.Installations.Db.Reference
                               (Reference);
         begin
            Installation.Add_Trade_Offers;
         end;
      end loop;

      Concorde.Worlds.Db.Scan
        (Concorde.Worlds.Has_Market'Access,
         Concorde.Worlds.Updates.Execute_Trades'Access);

      Concorde.Installations.Db.Iterate
        (Concorde.Installations.Production.Execute_Production'Access);

      Concorde.People.Pops.Db.Iterate
        (Concorde.People.Pops.Consumption.Execute_Consumption'Access);

   end Daily_Update;

end Concorde.Economy.Updates;
