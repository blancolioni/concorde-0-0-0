package body Concorde.Units.Configure is

   --------------------
   -- Configure_Unit --
   --------------------

   procedure Configure_Unit
     (Config : Tropos.Configuration)
   is
      procedure Create (Unit : in out Root_Unit_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Unit : in out Root_Unit_Type'Class) is
      begin
         Unit.Set_Local_Tag (Config.Config_Name);
         Unit.Movement := Movement_Category'Value (Config.Get ("movement"));
         Unit.Speed := Config.Get ("speed");
         Unit.Recon := Config.Get ("recon");
         Unit.Stealth := Config.Get ("stealth");
         Unit.Armour := Config.Get ("armour");
         for Attack of Config.Child ("attack") loop
            declare
               Weapon : constant Weapon_Category :=
                          Weapon_Category'Value (Attack.Config_Name);
            begin
               Unit.Weapons (Weapon).Strength := Attack.Value;
            end;
         end loop;
         Unit.Cargo := Config.Get ("cargo", 0);
         Unit.Can_Be_Cargo := Config.Get ("can-be-cargo", False);
         Unit.Combat := Config.Get ("combat", False);
         Unit.Rank := Config.Get ("rank", 0);
         Unit.Turns_To_Build := Config.Get ("turns-to-build", 0);
         Unit.Image_Resource :=
           Ada.Strings.Unbounded.To_Unbounded_String (Config.Get ("icon", ""));
      end Create;

   begin
      Db.Create (Create'Access);
   end Configure_Unit;

end Concorde.Units.Configure;
