private with Memor;
private with Memor.Database;

with Concorde.Agents;
with Concorde.Commodities;
with Concorde.Locations;
with Concorde.Managers;
with Concorde.Ownership;
with Concorde.Trades;

with Concorde.Money;
with Concorde.Quantities;

limited with Concorde.People.Communities;

private with Concorde.Commodities.Lists;

package Concorde.Corporations is

   type Corporation_Business_Type is
     (Import, Export, Banking);

   type Root_Corporation_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Managers.Managed_Interface
   with private;

   type Corporation_Type is access constant Root_Corporation_Type'Class;

   function Owner
     (Corporation : Root_Corporation_Type'Class)
      return access constant Concorde.Agents.Root_Agent_Type'Class;

   procedure Perform_Work
     (Corporation : in out Root_Corporation_Type'Class);

   type Updateable_Reference
     (Item : not null access Root_Corporation_Type'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Root_Corporation_Type'Class)
      return Updateable_Reference;

private

   type Root_Corporation_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Managers.Managed_Interface with
      record
         Manager      : Concorde.Managers.Manager_Type;
         Owner        : access constant
           Concorde.Agents.Root_Agent_Type'Class;
         Community    : access constant
           Concorde.People.Communities.Root_Community_Type'Class;
         Size         : Concorde.Quantities.Quantity_Type;
         Business     : Corporation_Business_Type;
         Commodities  : Concorde.Commodities.Lists.List;
         Requirements : Concorde.Commodities.Virtual_Stock_Type;
         Offered      : Concorde.Commodities.Virtual_Stock_Type;
         Sold         : Concorde.Commodities.Virtual_Stock_Type;
      end record;

   overriding procedure Update_Agent
     (Corporation           : not null access constant Root_Corporation_Type;
      Perform_Update : not null access
        procedure (Agent : in out Concorde.Agents.Root_Agent_Type'Class));

   overriding function Object_Database
     (Item : Root_Corporation_Type)
      return Memor.Memor_Database;

   overriding function Daily_Budget
     (Corporation  : Root_Corporation_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Money.Money_Type;

   overriding function Daily_Needs
     (Corporation  : Root_Corporation_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type;

   overriding function Daily_Supply
     (Corporation  : Root_Corporation_Type;
      Commodity    : Concorde.Commodities.Commodity_Type)
      return Concorde.Quantities.Quantity_Type;

   overriding function Identifier
     (Corporation : Root_Corporation_Type) return String;

   overriding function Short_Name
     (Corporation : Root_Corporation_Type) return String
   is (Corporation.Identifier);

   overriding function Class_Name
     (Corporation : Root_Corporation_Type) return String
   is ("corporation");

   overriding function Variable_Reference
     (Corporation : not null access constant Root_Corporation_Type)
      return access Concorde.Agents.Root_Agent_Type'Class
   is (Corporation.Update.Item);

   overriding function Manager
     (Corporation  : Root_Corporation_Type)
      return Concorde.Managers.Manager_Type
   is (Corporation.Manager);

   overriding procedure Set_Manager
     (Corporation    : in out Root_Corporation_Type;
      Manager     : Concorde.Managers.Manager_Type);

   function Owner
     (Corporation : Root_Corporation_Type'Class)
      return access constant Concorde.Agents.Root_Agent_Type'Class
   is (Corporation.Owner);

   package Db is
     new Memor.Database
       ("corporation", Root_Corporation_Type, Corporation_Type);

   overriding function Object_Database
     (Item : Root_Corporation_Type)
      return Memor.Memor_Database
   is (Db.Get_Database);

   type Updateable_Reference
     (Item : not null access Root_Corporation_Type'Class)
   is
      record
         Update : Db.Updateable_Reference (Item);
      end record;

end Concorde.Corporations;
