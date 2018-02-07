package Concorde.Regiments.Create is

   function New_Regiment
     (Pop  : Concorde.People.Pops.Pop_Type;
      Unit : Concorde.Units.Unit_Type)
      return Regiment_Type;

end Concorde.Regiments.Create;
