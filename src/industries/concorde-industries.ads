private with Memor;
private with Memor.Database;

with Concorde.Agents;
with Concorde.Commodities;
with Concorde.Locations;
with Concorde.Managers;
with Concorde.Ownership;
with Concorde.Production;
with Concorde.Trades;

with Concorde.Network;

with Concorde.Money;
with Concorde.Quantities;

package Concorde.Industries is

   type Root_Industry_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Managers.Managed_Interface
   with private;

   type Industry_Type is access constant Root_Industry_Type'Class;

   function Owner
     (Industry : Root_Industry_Type'Class)
      return access constant Concorde.Agents.Root_Agent_Type'Class;

   procedure Execute_Production
     (Industry : in out Root_Industry_Type'Class);

   type Updateable_Reference
     (Item : not null access Root_Industry_Type'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Root_Industry_Type'Class)
      return Updateable_Reference;

private

   type Root_Industry_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Managers.Managed_Interface with
      record
         Manager          : Concorde.Managers.Manager_Type;
         Owner            : access constant
           Concorde.Agents.Root_Agent_Type'Class;
         Node             : Concorde.Network.Node_State_Access;
         Size             : Non_Negative_Real;
         Production_Size  : Non_Negative_Real;
         Production       : Concorde.Production.Production_Type;
         Cost             : Concorde.Money.Money_Type;
         Produced         : Concorde.Commodities.Root_Stock_Type;
         Sold             : Concorde.Commodities.Root_Stock_Type;
         Production_Count : Natural := 0;
      end record;

   overriding procedure Update_Agent
     (Industry           : not null access constant Root_Industry_Type;
      Perform_Update : not null access
        procedure (Agent : in out Concorde.Agents.Root_Agent_Type'Class));

   overriding function Object_Database
     (Item : Root_Industry_Type)
      return Memor.Memor_Database;

   overriding function Daily_Budget
     (Industry  : Root_Industry_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Unit_Real;

   overriding function Daily_Needs
     (Industry  : Root_Industry_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Non_Negative_Real;

   overriding function Daily_Supply
     (Industry  : Root_Industry_Type;
      Commodity : Concorde.Commodities.Commodity_Type)
      return Non_Negative_Real;

   overriding procedure On_Commodity_Sell
     (Industry  : in out Root_Industry_Type;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type);

   overriding function Class_Name
     (Industry : Root_Industry_Type) return String
   is ("industry");

   overriding function Short_Name
     (Industry : Root_Industry_Type)
      return String
   is ("[" & Memor.To_String (Industry.Reference)
       & "-" & Industry.Production.Identifier
       & "]");

   overriding function Variable_Reference
     (Industry : not null access constant Root_Industry_Type)
      return access Concorde.Agents.Root_Agent_Type'Class
   is (Industry.Update.Item);

   overriding function Manager
     (Industry  : Root_Industry_Type)
      return Concorde.Managers.Manager_Type
   is (Industry.Manager);

   overriding procedure Set_Manager
     (Industry    : in out Root_Industry_Type;
      Manager     : Concorde.Managers.Manager_Type);

   function Owner
     (Industry : Root_Industry_Type'Class)
      return access constant Concorde.Agents.Root_Agent_Type'Class
   is (Industry.Owner);

   package Db is
     new Memor.Database
       ("industry", Root_Industry_Type, Industry_Type);

   overriding function Object_Database
     (Item : Root_Industry_Type)
      return Memor.Memor_Database
   is (Db.Get_Database);

   type Updateable_Reference
     (Item : not null access Root_Industry_Type'Class)
   is
      record
         Update : Db.Updateable_Reference (Item);
      end record;

end Concorde.Industries;
