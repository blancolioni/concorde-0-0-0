with Concorde.Locations;
with Concorde.Money;
with Concorde.Trades;

package Concorde.Installations.Create is

   function Create
     (Location      : Concorde.Locations.Object_Location;
      Market        : access constant Concorde.Trades.Trade_Interface'Class;
      Facility      : Concorde.Facilities.Facility_Type;
      Cash          : Concorde.Money.Money_Type;
      Owner         : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Size          : Concorde.Quantities.Quantity_Type)
      return Installation_Type;

end Concorde.Installations.Create;
