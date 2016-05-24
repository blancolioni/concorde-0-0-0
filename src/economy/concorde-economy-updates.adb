with Memor;

with Concorde.Agents;
with Concorde.Commodities;

with Concorde.People.Pops.Db;

with Concorde.Installations.Db;
with Concorde.Installations.Production;

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

      Virtual : constant Concorde.Commodities.Array_Of_Commodities :=
                  Concorde.Commodities.Get
                    (Concorde.Commodities.Virtual);

      procedure Agent_Trade_Update
        (Rec : in out Memor.Root_Record_Type'Class);

      procedure Ship_Trade_Update
        (Ship : in out Concorde.Ships.Root_Ship_Type'Class);

      ------------------------
      -- Agent_Trade_Update --
      ------------------------

      procedure Agent_Trade_Update
        (Rec : in out Memor.Root_Record_Type'Class)
      is
         Agent : Concorde.Agents.Root_Agent_Type'Class renames
                   Concorde.Agents.Root_Agent_Type'Class (Rec);
      begin
         for Commodity of Virtual loop
            Agent.Set_Quantity (Commodity, Quantities.Zero, Money.Zero);
         end loop;
         Agent.Before_Market;
         Agent.Add_Trade_Offers;
      end Agent_Trade_Update;

      -----------------------
      -- Ship_Trade_Update --
      -----------------------

      procedure Ship_Trade_Update
        (Ship : in out Concorde.Ships.Root_Ship_Type'Class)
      is
      begin
         Ship.Add_Trade_Offers;
      end Ship_Trade_Update;

   begin
      Concorde.Markets.Db.Iterate
        (Concorde.Markets.Before_Trading'Access);

      Concorde.People.Pops.Db.Iterate
        (Agent_Trade_Update'Access);

      Concorde.Installations.Db.Iterate
        (Agent_Trade_Update'Access);

      Concorde.Ships.Db.Iterate
        (Ship_Trade_Update'Access);

      Concorde.Worlds.Db.Scan
        (Concorde.Worlds.Has_Market'Access,
         Concorde.Worlds.Updates.Execute_Trades'Access);

      Concorde.Installations.Db.Iterate
        (Concorde.Installations.Production.Execute_Production'Access);

   end Daily_Update;

end Concorde.Economy.Updates;
