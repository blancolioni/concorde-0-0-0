with Concorde.Locations;
with Concorde.Trades;

with Concorde.Government;
with Concorde.People.Groups;

package Concorde.People.Pops.Create is

   function New_Pop
     (Market     : access constant Concorde.Trades.Trade_Interface'Class;
      Government : not null access constant
        Concorde.Government.Root_Government_Type'Class;
      Location   : Concorde.Locations.Object_Location;
      Group      : Concorde.People.Groups.Pop_Group;
      Size       : Pop_Size;
      Apathy     : Unit_Real)
      return Pop_Type;

end Concorde.People.Pops.Create;
