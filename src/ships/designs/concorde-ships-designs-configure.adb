with Ada.Text_IO;

with Tropos.Reader;

with Concorde.Configure;

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
         Ada.Text_IO.Put_Line ("component: " & Config.Config_Name);
         Design.Set_Name (Config.Get ("name", Config.Config_Name));
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
