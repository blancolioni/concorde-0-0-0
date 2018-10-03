with Concorde.Money;

with Concorde.Locations;
with Concorde.Trades;

with Concorde.Government;

package Concorde.Corporations.Create is

   function New_Corporation
     (Market          : not null access constant
        Concorde.Trades.Trade_Interface'Class;
      Government      : not null access constant
        Concorde.Government.Root_Government_Type'Class;
      Owner           : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Community       : not null access constant
        Concorde.People.Communities.Root_Community_Type'Class;
      Business        : Corporation_Business_Type;
      Commodities     : Concorde.Commodities.Array_Of_Commodities;
      Size            : Concorde.Quantities.Quantity_Type;
      Cash            : Concorde.Money.Money_Type)
      return Corporation_Type;

end Concorde.Corporations.Create;
