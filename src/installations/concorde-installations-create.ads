with Concorde.Money;

package Concorde.Installations.Create is

   function Create
     (Location : not null access constant
        Concorde.Agents.Agent_Location_Interface'Class;
      Facility : Concorde.Facilities.Facility_Type;
      Cash     : Concorde.Money.Money_Type;
      Owner    : not null access constant
        Concorde.Agents.Root_Agent_Type'Class)
      return Installation_Type;

end Concorde.Installations.Create;
