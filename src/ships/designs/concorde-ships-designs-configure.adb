with Ada.Text_IO;

with Tropos.Reader;

with Concorde.Configure;
with Concorde.Ships.Modules.Configure;

package body Concorde.Ships.Designs.Configure is

   procedure Configure_Design
     (Config : Tropos.Configuration);

   -------------------------
   -- Configure_Component --
   -------------------------

   procedure Configure_Design
     (Config : Tropos.Configuration)
   is
      procedure Create (Design : in out Root_Design_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Design : in out Root_Design_Type'Class) is
      begin
         Ada.Text_IO.Put_Line ("design: " & Config.Config_Name);
         Design.Identifier :=
           Ada.Strings.Unbounded.To_Unbounded_String (Config.Config_Name);
         Design.Set_Name (Config.Get ("name", Config.Config_Name));
         Design.Classification :=
           Ship_Classification'Value (Config.Get ("classification"));

         for Component_Config of Config.Child ("components") loop
            Design.Installed_Modules.Append
              (Installed_Module_Record'
                 (Module => Modules.Configure.Create_Module
                      (Component_Config)));
         end loop;
      end Create;

   begin
      Db.Create (Create'Access);
   end Configure_Design;

   -----------------------
   -- Configure_Designs --
   -----------------------

   procedure Configure_Designs is
      Base_Path : constant String :=
                    Concorde.Configure.Directory_Path ("ships/designs");
   begin
      Tropos.Reader.Read_Config
        (Path      => Base_Path,
         Extension => "txt",
         Configure => Configure_Design'Access);
   end Configure_Designs;

end Concorde.Ships.Designs.Configure;
