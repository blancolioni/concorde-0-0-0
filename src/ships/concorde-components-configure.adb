with Tropos.Reader;

with Concorde.Paths;

with Concorde.Components.Weapons;

with Concorde.Components.Manager;

package body Concorde.Components.Configure is

   procedure Configure_Component
     (Config : Tropos.Configuration);

   -------------------------
   -- Configure_Component --
   -------------------------

   procedure Configure_Component
     (Config : Tropos.Configuration)
   is
   begin
      if Config.Contains ("weapon") then
         declare
            Weapon : constant Concorde.Components.Weapons.Weapon_Component :=
                       Concorde.Components.Weapons.Configure_Weapon
                         (Config);
         begin
            Concorde.Components.Manager.Register
              (Config.Config_Name,
               Weapon);
         end;
      end if;
   end Configure_Component;

   --------------------------
   -- Configure_Components --
   --------------------------

   procedure Configure_Components is
   begin
      Tropos.Reader.Read_Config
        (Concorde.Paths.Config_File ("ships/components"),
         "txt",
         Configure_Component'Access);
   end Configure_Components;

end Concorde.Components.Configure;
