with Concorde.Locations;
with WL.Money;
with Concorde.Trades;

package Concorde.Installations.Create is

   function Create
     (Location      : Concorde.Locations.Object_Location;
      Market        : access constant Concorde.Trades.Trade_Interface'Class;
      Facility      : Concorde.Facilities.Facility_Type;
      Cash          : WL.Money.Money_Type;
      Owner         : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Size          : WL.Quantities.Quantity_Type)
      return Installation_Type;

end Concorde.Installations.Create;
