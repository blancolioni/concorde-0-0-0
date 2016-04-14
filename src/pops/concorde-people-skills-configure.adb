with Tropos.Reader;

with Concorde.Paths;

with Concorde.Commodities.Configure;

with Concorde.People.Skills.Db;

package body Concorde.People.Skills.Configure is

   procedure Create_Pop_Skill
     (Config : Tropos.Configuration);

   --------------------------
   -- Configure_Pop_Skills --
   --------------------------

   procedure Configure_Pop_Skills is
      Config : constant Tropos.Configuration :=
                 Tropos.Reader.Read_Config
                   (Concorde.Paths.Config_File
                      ("pops/pop_skills.txt"));
   begin
      for Skill_Config of Config loop
         Create_Pop_Skill (Skill_Config);
      end loop;
   end Configure_Pop_Skills;

   ----------------------
   -- Create_Pop_Skill --
   ----------------------

   procedure Create_Pop_Skill
     (Config : Tropos.Configuration)
   is
      Name  : constant String := Config.Config_Name;

      procedure Create (Skill : in out Root_Pop_Skill'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Skill : in out Root_Pop_Skill'Class) is
         Base_Pay : constant Real := Config.Get ("base_pay");
      begin
         Skill.Pop_Skill_Id := new String'(Name);
         Skill.Set_Name (Config.Get ("name", Name));
         Skill.Base_Pay := Concorde.Money.To_Price (Base_Pay);
      end Create;

      New_Skill : constant Pop_Skill :=
                    Db.Create (Create'Access);
   begin
      Concorde.Commodities.Configure.Create_From_Skill (New_Skill);
   end Create_Pop_Skill;

end Concorde.People.Skills.Configure;
