package body Concorde.Ships.Modules.Configure is

   -------------------
   -- Create_Module --
   -------------------

   function Create_Module
     (Config : Tropos.Configuration)
      return Module_Type
   is
      procedure Create (Module : in out Root_Module_Type'Class);

      procedure Create (Module : in out Root_Module_Type'Class) is
      begin
         Module.Set_Name (Config.Get ("name", Config.Config_Name));
         Module.Orientation := Xi.Matrices.Identity;
         Module.Component :=
           Concorde.Ships.Components.Get (Config.Get ("component"));

         if Config.Contains ("position") then
            declare
               Position : constant Tropos.Configuration :=
                            Config.Child ("position");
               X        : constant Float := Position.Get (1);
               Y        : constant Float := Position.Get (2);
               Z        : constant Float := Position.Get (3);
            begin
               Module.Position := (Real (X), Real (Y), Real (Z));
            end;
         else
            Module.Position := (0.0, 0.0, 0.0);
         end if;

      end Create;

      Module : constant Module_Type := Db.Create (Create'Access);
   begin
      return Module;
   end Create_Module;

end Concorde.Ships.Modules.Configure;
