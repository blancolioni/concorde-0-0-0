with Concorde.Locations;
with Concorde.Trades;

with Concorde.Government;

package Concorde.Industries.Create is

   function New_Industry
     (Market          : not null access constant
        Concorde.Trades.Trade_Interface'Class;
      Government      : not null access constant
        Concorde.Government.Root_Government_Type'Class;
      Production_Node : Concorde.Network.Node_State_Access;
      Inputs          : Concorde.Commodities.Array_Of_Commodities;
      Outputs         : Concorde.Commodities.Array_Of_Commodities)
      return Industry_Type;

end Concorde.Industries.Create;
