private with Memor;
private with Memor.Database;
private with Memor.Element_Vectors;

with Concorde.Agents;
with Concorde.Commodities;
with Concorde.Installations;
with Concorde.Locations;
with Concorde.Objects;
with Concorde.Powers;
with Concorde.Trades;

limited with Concorde.People.Individuals;

with WL.Money;
with WL.Quantities;

package Concorde.Government is

   type Root_Government_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Trades.Trade_Manager_Interface
     and Concorde.Powers.Powered_Interface
   with private;

   function Governor
     (Government : Root_Government_Type'Class)
      return access constant
     Concorde.People.Individuals.Root_Individual_Type'Class;

   procedure Set_Governor
     (Government : in out Root_Government_Type'Class;
      Governor   : access constant
        Concorde.People.Individuals.Root_Individual_Type'Class);

   function Headquarters
     (Government : Root_Government_Type'Class)
      return Concorde.Installations.Installation_Type;

   overriding function Contains
     (Government : Root_Government_Type;
      Power      : Concorde.Powers.Power_Type)
      return Boolean;

   overriding procedure Insert
     (Government : in out Root_Government_Type;
      Power      : Concorde.Powers.Power_Type);

   overriding procedure Remove
     (Government : in out Root_Government_Type;
      Power      : Concorde.Powers.Power_Type);

   overriding function Check_Powers
     (Government : Root_Government_Type;
      Test       : not null access
        function (Power : Concorde.Powers.Power_Type) return Boolean)
      return Boolean;

   overriding procedure Scan_Powers
     (Government : Root_Government_Type;
      Process   : not null access
        procedure (Power : Concorde.Powers.Power_Type));

   overriding function Tax_Rate
     (Government : Root_Government_Type;
      Category   : Concorde.Trades.Market_Tax_Category;
      Commodity  : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class)
      return Unit_Real;

   overriding procedure Tax_Receipt
     (Government : Root_Government_Type;
      Commodity  : Concorde.Commodities.Commodity_Type;
      Quantity   : WL.Quantities.Quantity_Type;
      Price      : WL.Money.Price_Type;
      Category   : Concorde.Trades.Market_Tax_Category;
      Receipt    : WL.Money.Money_Type);

   procedure Set_Tax_Rate
     (Government : in out Root_Government_Type'Class;
      Category   : Concorde.Trades.Market_Tax_Category;
      Commodity  : not null access constant
        Concorde.Commodities.Root_Commodity_Type'Class;
      Rate       : Unit_Real);

   function Base_Tax_Rate
     (Government : Root_Government_Type'Class;
      Category   : Concorde.Trades.Market_Tax_Category)
      return Unit_Real;

   procedure Set_Base_Tax_Rate
     (Government : in out Root_Government_Type'Class;
      Category   : Concorde.Trades.Market_Tax_Category;
      Rate       : Unit_Real);

   function Tax_Receipts
     (Government : Root_Government_Type'Class;
      Category   : Concorde.Trades.Market_Tax_Category)
      return WL.Money.Money_Type;

   function Basic_Living_Wage
     (Government : Root_Government_Type'Class)
      return WL.Money.Price_Type;

   procedure Set_Basic_Living_Wage
     (Government : in out Root_Government_Type'Class;
      Wage       : WL.Money.Price_Type);

   function Slavery_Allowed
     (Government : Root_Government_Type'Class)
      return Boolean;

   type Government_Type is access constant Root_Government_Type'Class;

   type Governed_Interface is limited interface
     and Concorde.Objects.Named_Object_Interface;

   function Government
     (Governed : Governed_Interface)
      return Government_Type
      is abstract;

   function Governed
     (Government : Root_Government_Type'Class)
      return access constant Governed_Interface'Class;

   function Get_Government
     (Location : Concorde.Locations.Object_Location)
      return Government_Type;

   type Updateable_Reference
     (Item : not null access Root_Government_Type'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Root_Government_Type'Class)
      return Updateable_Reference;

private

   type Array_Of_Tax_Rates is
     array (Concorde.Trades.Market_Tax_Category) of Unit_Real;

   Default_Tax_Rates : constant Array_Of_Tax_Rates :=
                         (Concorde.Trades.Sales  => 0.0,
                          Concorde.Trades.Import => 0.0,
                          Concorde.Trades.Export => 0.0);

   type Array_Of_Tax_Receipts is
     array (Concorde.Trades.Market_Tax_Category) of WL.Money.Money_Type;

   package Commodity_Tax_Rates is
     new Memor.Element_Vectors
       (Concorde.Commodities.Root_Commodity_Type,
        Array_Of_Tax_Rates, Default_Tax_Rates);

   type Root_Government_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Trades.Trade_Manager_Interface
     and Concorde.Powers.Powered_Interface with
      record
         Governed          : access constant Governed_Interface'Class;
         Owner             : access constant
           Concorde.Agents.Root_Agent_Type'Class;
         Governor          : access constant
           Concorde.People.Individuals.Root_Individual_Type'Class;
         Headquarters      : Concorde.Installations.Installation_Type;
         Powers            : Concorde.Powers.Power_Set;
         Base_Tax_Rate     : Array_Of_Tax_Rates := Default_Tax_Rates;
         Tax_Rates         : Commodity_Tax_Rates.Vector;
         Tax_Receipts      : Array_Of_Tax_Receipts :=
                               (others => WL.Money.Zero);
         Owner_Tithe       : Unit_Real := 0.1;
         Basic_Living_Wage : WL.Money.Price_Type := WL.Money.Zero;
         Slavery_Allowed   : Boolean := False;
      end record;

   overriding function Class_Name
     (Government : Root_Government_Type)
      return String
   is ("government");

   overriding function Object_Database
     (Government : Root_Government_Type)
      return Memor.Memor_Database;

   overriding function Short_Name
     (Government : Root_Government_Type)
      return String
   is (Government.Governed.Name & " Government");

--     overriding procedure Add_Trade_Offers
--       (Item   : not null access constant Root_Government_Type);

   overriding function Variable_Reference
     (Government : not null access constant Root_Government_Type)
      return access Concorde.Agents.Root_Agent_Type'Class
   is (Government.Update.Item);

   overriding function Contains
     (Government : Root_Government_Type;
      Power      : Concorde.Powers.Power_Type)
      return Boolean
   is (Government.Powers.Contains (Power));

   function Governor
     (Government : Root_Government_Type'Class)
      return access constant
     Concorde.People.Individuals.Root_Individual_Type'Class
   is (Government.Governor);

   function Basic_Living_Wage
     (Government : Root_Government_Type'Class)
      return WL.Money.Price_Type
   is (Government.Basic_Living_Wage);

   function Slavery_Allowed
     (Government : Root_Government_Type'Class)
      return Boolean
   is (Government.Slavery_Allowed);

   function Base_Tax_Rate
     (Government : Root_Government_Type'Class;
      Category   : Concorde.Trades.Market_Tax_Category)
      return Unit_Real
   is (Government.Base_Tax_Rate (Category));

   overriding function Check_Powers
     (Government : Root_Government_Type;
      Test       : not null access
        function (Power : Concorde.Powers.Power_Type) return Boolean)
      return Boolean
   is (Government.Powers.Check_Powers (Test));

   package Db is
     new Memor.Database
       ("government", Root_Government_Type, Government_Type);

   type Updateable_Reference
     (Item : not null access Root_Government_Type'Class)
   is
      record
         Update : Db.Updateable_Reference (Item);
      end record;

end Concorde.Government;
