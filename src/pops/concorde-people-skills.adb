package body Concorde.People.Skills is

   ----------
   -- Scan --
   ----------

   overriding procedure Scan
     (Set     : Skill_Set;
      Process : not null access
        procedure (Skill : Skill_Type;
                   Level : Skill_Level))
   is
   begin
      for Position in Set.Iterate loop
         if Skill_Maps.Element (Position) > 0 then
            Process (Get (Skill_Maps.Key (Position)),
                     Skill_Maps.Element (Position));
         end if;
      end loop;
   end Scan;

end Concorde.People.Skills;
