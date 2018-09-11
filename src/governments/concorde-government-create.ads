with WL.Money;

package Concorde.Government.Create is

   function Create_Government
     (Governed : not null access constant Governed_Interface'Class;
      Location : Concorde.Locations.Object_Location;
      Cash     : WL.Money.Money_Type;
      Owner    : not null access constant
        Concorde.Agents.Root_Agent_Type'Class)
      return Government_Type;

end Concorde.Government.Create;
