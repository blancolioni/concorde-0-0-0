private with Memor.Database;

with Concorde.Agents;
with Concorde.Commodities;
with Concorde.Locations;
with Concorde.Objects;

with Concorde.Calendar;

with WL.Money;
with WL.Quantities;

package Concorde.Contracts is

   type Root_Contract_Type is
     new Concorde.Objects.Root_Object_Type with private;

   type Contract_Type is access constant Root_Contract_Type'Class;

   function New_Buy_Contract
     (Location  : Concorde.Locations.Object_Location;
      Buyer     : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : WL.Quantities.Quantity_Type;
      Price     : WL.Money.Price_Type;
      Expires   : Concorde.Calendar.Time)
      return Contract_Type;

   procedure Scan_Contracts
     (Check : not null access
        procedure (Contract : Contract_Type));

   procedure Accept_Contract
     (Agent    : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Contract : Contract_Type);

   procedure Complete_Contract
     (Contract : Contract_Type);

   procedure Cancel_Contract
     (Contract : Contract_Type);

   type Updateable_Reference
     (Item : not null access Root_Contract_Type'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Root_Contract_Type'Class)
      return Updateable_Reference;

private

   type Contract_Class is (Buy_Goods);

   type Root_Contract_Type is
     new Concorde.Objects.Root_Object_Type with
      record
         Class       : Contract_Class;
         Location    : Concorde.Locations.Object_Location;
         Offered_By  : Concorde.Agents.Agent_Type;
         Accepted_By : Concorde.Agents.Agent_Type;
         Commodity   : Concorde.Commodities.Commodity_Type;
         Quantity    : WL.Quantities.Quantity_Type;
         Price       : WL.Money.Price_Type;
         Active      : Boolean;
         Canceled    : Boolean;
         Issued      : Concorde.Calendar.Time;
         Accepted    : Concorde.Calendar.Time;
         Completed   : Concorde.Calendar.Time;
         Expires     : Concorde.Calendar.Time;
      end record;

   overriding function Object_Database
     (Item : Root_Contract_Type)
      return Memor.Memor_Database;

   package Db is
     new Memor.Database
       ("contract", Root_Contract_Type, Contract_Type);

   overriding function Object_Database
     (Item : Root_Contract_Type)
      return Memor.Memor_Database
   is (Db.Get_Database);

   type Updateable_Reference
     (Item : not null access Root_Contract_Type'Class)
   is
      record
         Update : Db.Updateable_Reference (Item);
      end record;

end Concorde.Contracts;
