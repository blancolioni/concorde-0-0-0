with Ada.Exceptions;

with Concorde.People.Attributes.Configure;

package body Concorde.People.Careers.Configure is

   procedure Create_Career
     (Config : Tropos.Configuration);

--     procedure Configure_
--       (List   : out Qualification_Lists.List;
--        Config : Tropos.Configuration);

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

   -----------------------------
   -- Configure_Qualification --
   -----------------------------

--     procedure Configure_Qualification
--       (List   : out Qualification_Lists.List;
--        Config : Tropos.Configuration)
--     is
--     begin
--        for Qual_Config of Config loop
--           declare
--              Name  : constant String := Qual_Config.Config_Name;
--              Bonus : constant Boolean := Qual_Config.Child_Count = 0;
--              Value : constant Natural :=
--                        (if Bonus then 0 else Qual_Config.Value);
--           begin
--              if Name = "education" then
--                 List.Append ((Education, Bonus, Value));
--              elsif Concorde.People.Skills.Exists (Name) then
--                 List.Append
--                   ((Skill_Check, Bonus,
--                    Concorde.People.Skills.Get (Name),
--                    Concorde.People.Skills.Skill_Level (Value)));
--              else
--                 begin
--                    List.Append
--                      ((Ability_Check, Bonus,
--                       Concorde.People.Abilities.Ability_Type'Value (Name),
--                       Concorde.People.Abilities.Ability_Score_Range
--                         (Value)));
--                 exception
--                    when Constraint_Error =>
--                       raise Constraint_Error with
--                         "no such qualification type: " & Name;
--                 end;
--              end if;
--           end;
--        end loop;
--     end Configure_Qualification;

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
            Concorde.People.Attributes.Configure.Configure_Attributes
              (Career.Qualifications, Config.Child ("qualification"));
         end if;

         if Config.Contains ("promotion") then
            Concorde.People.Attributes.Configure.Configure_Attributes
              (Career.Promotion, Config.Child ("promotion"));
         end if;

         if Config.Contains ("advanced") then
            Concorde.People.Attributes.Configure.Configure_Attributes
              (Career.Advanced_Check, Config.Child ("advanced"));
         end if;

         if Config.Contains ("progression") then
            declare
               Prog_Config : constant Tropos.Configuration :=
                               Config.Child ("progression");
            begin
               if Prog_Config.Contains ("development") then
                  Concorde.People.Attributes.Configure.Configure_Attributes
                    (Career.Development, Prog_Config.Child ("development"));
               end if;
               if Prog_Config.Contains ("service") then
                  Concorde.People.Attributes.Configure.Configure_Attributes
                    (Career.Service, Prog_Config.Child ("service"));
               end if;
               if Prog_Config.Contains ("specialist") then
                  Concorde.People.Attributes.Configure.Configure_Attributes
                    (Career.Specialist, Prog_Config.Child ("specialist"));
               end if;
               if Prog_Config.Contains ("advanced") then
                  Concorde.People.Attributes.Configure.Configure_Attributes
                    (Career.Advanced, Prog_Config.Child ("advanced"));
               end if;
            end;
         end if;

         Career.Ranks :=
           new Array_Of_Ranks (1 .. Number_Of_Ranks);

         declare
            Index : Rank_Count := 0;
         begin
            for Rank_Config of Config.Child ("ranks") loop
               Index := Index + 1;

               declare
                  Rank : Rank_Record renames Career.Ranks (Index);
               begin
                  Rank.Name := new String'
                    (Rank_Config.Get ("name", Rank_Config.Config_Name));
                  if Rank_Config.Contains ("progression") then
                     Concorde.People.Attributes.Configure.Configure_Attributes
                       (Rank.Progression, Rank_Config.Child ("progression"));
                  end if;
               end;
            end loop;
         end;

      exception
         when E : others =>
            raise Constraint_Error with
              "error configuring career "
              & Config.Config_Name
              & ": " & Ada.Exceptions.Exception_Message (E);

      end Create;

   begin
      Db.Create (Create'Access);
   end Create_Career;

end Concorde.People.Careers.Configure;
