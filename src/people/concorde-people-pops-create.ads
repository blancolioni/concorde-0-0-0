with Concorde.Locations;
with WL.Money;
with Concorde.Trades;

with Concorde.People.Groups;

package Concorde.People.Pops.Create is

   function New_Pop
     (Location : Concorde.Locations.Object_Location;
      Market   : access constant Concorde.Trades.Trade_Interface'Class;
      Group    : Concorde.People.Groups.Pop_Group;
      Size     : Pop_Size;
      Cash     : WL.Money.Money_Type)
      return Pop_Type;

end Concorde.People.Pops.Create;
