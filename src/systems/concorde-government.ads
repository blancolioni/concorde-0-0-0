private with Memor;
private with Memor.Element_Vectors;

with Concorde.Agents;
with Concorde.Commodities;
with Concorde.Installations;
with Concorde.Objects;
with Concorde.Trades;

with Concorde.Money;
with Concorde.Quantities;

package Concorde.Government is

   type Governed_Interface is limited interface
     and Concorde.Objects.Named_Object_Interface;

   type Root_Government_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Trades.Trade_Manager_Interface
   with private;

   function Governed
     (Government : Root_Government_Type'Class)
      return access constant Governed_Interface'Class;

   overriding function Tax_Rate
     (Government : Root_Government_Type;
      Category   : Concorde.Trades.Market_Tax_Category;
      Commodity  : Concorde.Commodities.Commodity_Type)
      return Unit_Real;

   overriding procedure Tax_Receipt
     (Government : in out Root_Government_Type;
      Commodity  : Concorde.Commodities.Commodity_Type;
      Quantity   : Concorde.Quantities.Quantity;
      Price      : Concorde.Money.Price_Type;
      Category   : Concorde.Trades.Market_Tax_Category;
      Receipt    : Concorde.Money.Money_Type);

   function Basic_Living_Wage
     (Government : Root_Government_Type'Class)
      return Boolean;

   type Government_Type is access constant Root_Government_Type'Class;

private

   type Array_Of_Tax_Rates is
     array (Concorde.Trades.Market_Tax_Category) of Unit_Real;

   Default_Tax_Rates : constant Array_Of_Tax_Rates :=
                         (Concorde.Trades.Sales  => 0.1,
                          Concorde.Trades.Import => 0.1,
                          Concorde.Trades.Export => 0.1);

   package Commodity_Tax_Rates is
     new Memor.Element_Vectors (Array_Of_Tax_Rates, Default_Tax_Rates);

   type Root_Government_Type is
     new Concorde.Agents.Root_Agent_Type
     and Concorde.Trades.Trade_Manager_Interface with
      record
         Governed          : access constant Governed_Interface'Class;
         Owner             : access constant
           Concorde.Agents.Root_Agent_Type'Class;
         Headquarters      : Concorde.Installations.Installation_Type;
         Tax_Rates         : Commodity_Tax_Rates.Vector;
         Basic_Living_Wage : Boolean := False;
      end record;

   overriding function Object_Database
     (Government : Root_Government_Type)
      return Memor.Root_Database_Type'Class;

   overriding function Short_Name
     (Government : Root_Government_Type)
      return String
   is (Government.Governed.Name & " Government");

   overriding procedure Add_Trade_Offers
     (Item   : not null access constant Root_Government_Type;
      Market : in out Concorde.Trades.Trade_Interface'Class);

end Concorde.Government;
