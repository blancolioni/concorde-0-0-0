package body Concorde.People.Skills is

   ----------
   -- Scan --
   ----------

   overriding procedure Scan
     (Set     : Skill_Set;
      Process : not null access
        procedure (Skill : Skill_Type;
                   Level : Skill_Level_Range))
   is
   begin
      for Position in Set.Iterate loop
         if Skill_Maps.Element (Position) > 0 then
            Process (Get (Skill_Maps.Key (Position)),
                     Skill_Maps.Element (Position));
         end if;
      end loop;
   end Scan;

   ---------------
   -- Set_Level --
   ---------------

   procedure Set_Level
     (Set   : in out Skill_Set'Class;
      Skill : not null access constant Root_Skill_Type'Class;
      Level : Skill_Level_Range)
   is
   begin
      if not Set.Contains (Skill.Identifier) then
         Set.Insert (Skill.Identifier, Level);
      else
         Set.Replace (Skill.Identifier, Level);
      end if;
   end Set_Level;

end Concorde.People.Skills;
