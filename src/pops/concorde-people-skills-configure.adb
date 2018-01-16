with Ada.Text_IO;

package body Concorde.People.Skills.Configure is

   procedure Create_Skill
     (Config : Tropos.Configuration);

   ----------------------
   -- Configure_Skills --
   ----------------------

   procedure Configure_Skills
     (Config : Tropos.Configuration)
   is
   begin
      for Skill_Config of Config loop
         Create_Skill (Skill_Config);
      end loop;
   end Configure_Skills;

   ------------------
   -- Create_Skill --
   ------------------

   procedure Create_Skill
     (Config : Tropos.Configuration)
   is
      Name  : constant String := Config.Config_Name;

      procedure Create (Skill : in out Root_Skill_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Skill : in out Root_Skill_Type'Class) is
      begin
         Skill.Set_Local_Tag (Name);
         Ada.Text_IO.Put_Line ("new skill: " & Name);
      end Create;

   begin
      Db.Create (Create'Access);
   end Create_Skill;

end Concorde.People.Skills.Configure;
