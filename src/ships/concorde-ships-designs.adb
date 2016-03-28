with Tropos.Reader;

with Concorde.Hash_Table;
with Concorde.Paths;

with Concorde.Components;
with Concorde.Components.Manager;

package body Concorde.Ships.Designs is

   package Component_Vectors is
     new Ada.Containers.Vectors
       (Positive, Concorde.Components.Component_Type, Concorde.Components."=");

   type Ship_Design is
      record
         Components : Component_Vectors.Vector;
      end record;

   package Ship_Design_Tables is
     new Concorde.Hash_Table (Ship_Design);

   Designs : Ship_Design_Tables.Map;

   procedure Configure_Design
     (Config : Tropos.Configuration);

   ----------------------
   -- Configure_Design --
   ----------------------

   procedure Configure_Design
     (Config : Tropos.Configuration)
   is
      Design : Ship_Design;
   begin
      for Item of Config loop
         Design.Components.Append
           (Concorde.Components.Manager.Get (Item.Config_Name));
      end loop;
      Designs.Insert (Config.Config_Name, Design);
   end Configure_Design;

   -----------------------
   -- Configure_Designs --
   -----------------------

   procedure Configure_Designs is
   begin
      Tropos.Reader.Read_Config
        (Concorde.Paths.Config_File ("ships/designs"),
         "txt",
         Configure_Design'Access);
   end Configure_Designs;

   -----------------------------
   -- Create_Ship_From_Design --
   -----------------------------

   function Create_Ship_From_Design
     (Name : String)
      return Ship_Type
   is
      Design : Ship_Design renames Designs.Element (Name);
      Ship   : constant Ship_Type := new Root_Ship_Type;
   begin
      Ship.Size := 0;
      Ship.Empty_Mass := 0;
      for Component of Design.Components loop
         declare
            M : constant Concorde.Modules.Module_Type :=
                  Concorde.Modules.New_Module (Component);
         begin
            Ship.Structure.Append (M);
            Ship.Size := Ship.Size + M.Size;
            Ship.Empty_Mass := Ship.Empty_Mass + M.Mass;
         end;
      end loop;
      return Ship;
   end Create_Ship_From_Design;

end Concorde.Ships.Designs;
