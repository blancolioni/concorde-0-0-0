package body Concorde.People.Careers.Configure is

   procedure Create_Career
     (Config : Tropos.Configuration);

   ----------------------
   -- Configure_Careers --
   ----------------------

   procedure Configure_Careers
     (Config : Tropos.Configuration)
   is
   begin
      for Career_Config of Config loop
         Create_Career (Career_Config);
      end loop;
   end Configure_Careers;

   ------------------
   -- Create_Career --
   ------------------

   procedure Create_Career
     (Config : Tropos.Configuration)
   is
      Name  : constant String := Config.Config_Name;

      procedure Create (Career : in out Root_Career_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Career : in out Root_Career_Type'Class) is
         Number_Of_Ranks : constant Rank_Count :=
                             Rank_Count
                               (Integer'
                                  (Config.Child ("ranks").Child_Count));
      begin
         Career.Set_Local_Tag (Name);

         Career.Prestige := Config.Get ("prestige");
         Career.Titles := Config.Get ("titles");

         if Config.Contains ("qualification") then
            for Qual_Config of Config.Child ("qualification") loop
               declare
                  Name  : constant String := Qual_Config.Config_Name;
                  Value : constant Natural := Qual_Config.Value;
               begin
                  if Name = "education" then
                     Career.Qualifications.Append
                       ((Education, Value));
                  elsif Concorde.People.Skills.Exists (Name) then
                     Career.Qualifications.Append
                       ((Skill_Check,
                        Concorde.People.Skills.Get (Name),
                        Concorde.People.Skills.Skill_Level (Value)));
                  else
                     begin
                        Career.Qualifications.Append
                          ((Ability_Check,
                           Concorde.People.Abilities.Ability_Type'Value (Name),
                           Concorde.People.Abilities.Ability_Score_Range
                             (Value)));
                     exception
                        when Constraint_Error =>
                           raise Constraint_Error with
                           "career: " & Career.Name
                             & ": no such qualification type: " & Name;
                     end;
                  end if;
               end;
            end loop;
         end if;

         Career.Ranks :=
           new Array_Of_Ranks (1 .. Number_Of_Ranks);

         declare
            Index : Rank_Count := 0;
         begin
            for Rank_Config of Config.Child ("ranks") loop
               Index := Index + 1;
               declare
                  Number_Of_Skills : constant Natural :=
                                       (if Rank_Config.Contains ("skills")
                                        then Rank_Config.Child ("skills")
                                        .Child_Count
                                        else 0);
               begin
                  Career.Ranks (Index) :=
                    Rank_Record'
                      (Name   =>
                          new String'
                         (Rank_Config.Get ("name", Rank_Config.Config_Name)),
                       Skills =>
                          new Array_Of_Skills (1 .. Number_Of_Skills));
                  if Rank_Config.Contains ("skills") then
                     declare
                        Skill_Index : Natural := 0;
                     begin
                        for Skill_Config of Rank_Config.Child ("skills") loop
                           Skill_Index := Skill_Index + 1;
                           if Concorde.People.Skills.Exists
                             (Skill_Config.Config_Name)
                           then
                              Career.Ranks (Index).Skills (Skill_Index) :=
                                Concorde.People.Skills.Get
                                  (Skill_Config.Config_Name);
                           else
                              raise Constraint_Error with
                                "career " & Career.Identifier
                                & ": rank " & Career.Ranks (Index).Name.all
                                & ": no such skill: "
                                & Skill_Config.Config_Name;
                           end if;
                        end loop;
                     end;
                  end if;
               end;
            end loop;
         end;
      end Create;

   begin
      Db.Create (Create'Access);
   end Create_Career;

end Concorde.People.Careers.Configure;
