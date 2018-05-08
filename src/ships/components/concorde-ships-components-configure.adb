with Ada.Text_IO;

with Tropos.Reader;

with Concorde.Configure;

package body Concorde.Ships.Components.Configure is

   procedure Configure_Component
     (Config : Tropos.Configuration);

   -------------------------
   -- Configure_Component --
   -------------------------

   procedure Configure_Component
     (Config : Tropos.Configuration)
   is
      procedure Create (Component : in out Root_Component_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Component : in out Root_Component_Type'Class) is
      begin
         Ada.Text_IO.Put_Line ("component: " & Config.Config_Name);
         Component.Set_Local_Tag (Config.Config_Name);
      end Create;

   begin
      Db.Create (Create'Access);
   end Configure_Component;

   --------------------------
   -- Configure_Components --
   --------------------------

   procedure Configure_Components is
      Base_Path : constant String :=
                    Concorde.Configure.Directory_Path ("ships/components");
   begin
      Tropos.Reader.Read_Config
        (Path      => Base_Path,
         Extension => "txt",
         Configure => Configure_Component'Access);
   end Configure_Components;

end Concorde.Ships.Components.Configure;
