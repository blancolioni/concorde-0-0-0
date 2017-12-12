private with Memor.Database;

with Concorde.Commodities;
with Concorde.Locations;
with Concorde.Objects;

with Concorde.Calendar;

with WL.Money;
with WL.Quantities;

package Concorde.Contracts is

   type Contract_Class is (Buy_Goods);

   type Root_Contract_Type is
     new Concorde.Objects.Root_Object_Type with private;

   function Class
     (Contract : Root_Contract_Type'Class)
      return Contract_Class;

   function Location
     (Contract : Root_Contract_Type'Class)
      return Concorde.Locations.Object_Location;

   function Commodity
     (Contract : Root_Contract_Type'Class)
      return Concorde.Commodities.Commodity_Type;

   function Quantity
     (Contract : Root_Contract_Type'Class)
      return WL.Quantities.Quantity_Type;

   function Total_Cost
     (Contract : Root_Contract_Type'Class)
      return WL.Money.Money_Type;

   function Price
     (Contract : Root_Contract_Type'Class)
      return WL.Money.Price_Type;

   function Show
     (Contract : Root_Contract_Type'Class)
      return String;

   type Contract_Type is access constant Root_Contract_Type'Class;

   procedure Scan_Available_Contracts
     (Check : not null access
        procedure (Contract : Contract_Type));

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

   type Contractor_Interface is limited interface;

   function Contracted_To_Buy
     (Contractor : Contractor_Interface;
      Commodity  : Concorde.Commodities.Commodity_Type)
      return WL.Quantities.Quantity_Type
      is abstract;

   function Contracted_Quantity
     (Contractor : Contractor_Interface)
      return WL.Quantities.Quantity_Type
      is abstract;

   procedure Add_Contract
     (Contractor : in out Contractor_Interface;
      Contract   : Contract_Type)
   is abstract;

   procedure Accept_Contract
     (Contractor : not null access constant Contractor_Interface'Class;
      Contract   : Contract_Type);

   function New_Buy_Contract
     (Location  : Concorde.Locations.Object_Location;
      Buyer     : not null access constant Contractor_Interface'Class;
      Commodity : Concorde.Commodities.Commodity_Type;
      Quantity  : WL.Quantities.Quantity_Type;
      Price     : WL.Money.Price_Type;
      Expires   : Concorde.Calendar.Time)
      return Contract_Type;

private

   type Contractor_Type is access constant Contractor_Interface'Class;

   type Root_Contract_Type is
     new Concorde.Objects.Root_Object_Type with
      record
         Class       : Contract_Class;
         Location    : Concorde.Locations.Object_Location;
         Offered_By  : Contractor_Type;
         Accepted_By : Contractor_Type;
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

   function Class
     (Contract : Root_Contract_Type'Class)
      return Contract_Class
   is (Contract.Class);

   function Location
     (Contract : Root_Contract_Type'Class)
      return Concorde.Locations.Object_Location
   is (Contract.Location);

   function Offered_By
     (Contract : Root_Contract_Type'Class)
      return access constant Contractor_Interface'Class
   is (Contract.Offered_By);

   function Commodity
     (Contract : Root_Contract_Type'Class)
      return Concorde.Commodities.Commodity_Type
   is (Contract.Commodity);

   function Quantity
     (Contract : Root_Contract_Type'Class)
      return WL.Quantities.Quantity_Type
   is (Contract.Quantity);

   function Price
     (Contract : Root_Contract_Type'Class)
      return WL.Money.Price_Type
   is (Contract.Price);

   function Total_Cost
     (Contract : Root_Contract_Type'Class)
      return WL.Money.Money_Type
   is (WL.Money.Total (Contract.Price, Contract.Quantity));

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
