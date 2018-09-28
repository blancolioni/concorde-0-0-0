with Concorde.Money;

with Concorde.Locations;
with Concorde.Trades;

with Concorde.Government;

package Concorde.Industries.Create is

   function New_Industry
     (Market          : not null access constant
        Concorde.Trades.Trade_Interface'Class;
      Government      : not null access constant
        Concorde.Government.Root_Government_Type'Class;
      Owner           : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Community       : not null access constant
        Concorde.People.Communities.Root_Community_Type'Class;
      Production      : not null access constant
        Concorde.Production.Root_Production_Type'Class;
      Size            : Non_Negative_Real;
      Cash            : Concorde.Money.Money_Type)
      return Industry_Type;

end Concorde.Industries.Create;
