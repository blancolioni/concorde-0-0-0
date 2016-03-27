package Concorde.Empires.Relations is

   function Has_Conflict
     (Es : Array_Of_Empires)
      return Boolean;

   function At_War
     (E1, E2 : not null access constant Root_Empire_Type'Class)
      return Boolean;

end Concorde.Empires.Relations;
