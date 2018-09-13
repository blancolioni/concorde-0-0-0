with Concorde.Locations;
with Concorde.Trades;

with Concorde.Government;
with Concorde.People.Groups;

package Concorde.People.Pops.Create is

   function New_Pop
     (Market     : access constant Concorde.Trades.Trade_Interface'Class;
      Government : not null access constant
        Concorde.Government.Root_Government_Type'Class;
      Network    : Concorde.Network.Network_State_Interface'Class;
      Groups     : Concorde.People.Groups.Array_Of_Pop_Groups;
      Size       : Pop_Size;
      Apathy     : Unit_Real)
      return Pop_Type;

end Concorde.People.Pops.Create;
