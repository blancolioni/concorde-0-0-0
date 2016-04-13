package Concorde.Installations.Create is

   function Create
     (Facility : Concorde.Facilities.Facility_Type;
      Owner    : not null access constant
        Concorde.Agents.Root_Agent_Type'Class)
      return Installation_Type;

end Concorde.Installations.Create;
