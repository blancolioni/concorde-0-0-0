package body Concorde.Empires.Relations is

   ------------
   -- At_War --
   ------------

   function At_War
     (E1, E2 : Root_Empire_Type'Class)
      return Boolean
   is
   begin
      return E1.Relationship (E2) < 0
        or else E2.Relationship (E1) < 0;
   end At_War;

   ------------------
   -- Has_Conflict --
   ------------------

   function Has_Conflict
     (Es : Array_Of_Empires)
      return Boolean
   is
   begin
      for I in 2 .. Es'Last loop
         for J in 1 .. I - 1 loop
            if Es (I).Relationship (Es (J).all) < 0
              or else Es (J).Relationship (Es (I).all) < 0
            then
               return True;
            end if;
         end loop;
      end loop;
      return False;
   end Has_Conflict;

end Concorde.Empires.Relations;
