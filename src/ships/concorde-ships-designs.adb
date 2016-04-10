with Ada.Strings.Unbounded;

with Tropos.Reader;

with Concorde.Hash_Table;
with Concorde.Paths;

with Concorde.Components;
with Concorde.Components.Manager;

package body Concorde.Ships.Designs is

   type Component_Layout_Record is
      record
         Component    : Concorde.Components.Component_Type;
         Name         : Ada.Strings.Unbounded.Unbounded_String;
         Left_Low_Aft : Module_Position;
         Size         : Size_Type;
         Orientation  : Module_Orientation;
      end record;

   package Component_Vectors is
     new Ada.Containers.Vectors
       (Positive, Component_Layout_Record);

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
      for Cfg of Config loop
         declare
            Component_Name : constant String :=
                               Cfg.Get ("component");
            Component      : constant Concorde.Components.Component_Type :=
                               Concorde.Components.Manager.Get
                                 (Component_Name);
            Module_Name    : constant String :=
                               Cfg.Get ("name", Component_Name);
            Size           : constant Size_Type :=
                               (X => Cfg.Get ("width"),
                                Y => Cfg.Get ("height"),
                                Z => Cfg.Get ("length"));
            Position       : constant Module_Position :=
                               (X => Cfg.Get ("left"),
                                Y => Cfg.Get ("low"),
                                Z => Cfg.Get ("aft"));
            Orientation    : Module_Orientation :=
                               (Axis => Z_Axis, Forward => True);
         begin
            if Cfg.Contains ("orientation") then
               declare
                  Orn : constant Tropos.Configuration :=
                          Cfg.Child ("orientation");
               begin
                  for I in 1 .. 3 loop
                     if Orn.Get (I) /= 0 then
                        Orientation.Axis :=
                          Orientation_Axis'Val (I - 1);
                        Orientation.Forward := Orn.Get (I) > 0;
                        exit;
                     end if;
                  end loop;
               end;
            end if;

            declare
               use Ada.Strings.Unbounded;
               New_Item : constant Component_Layout_Record :=
                            (Component, To_Unbounded_String (Module_Name),
                             Position, Size, Orientation);
            begin
               Design.Components.Append (New_Item);
            end;
         end;

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

   procedure Create_Ship_From_Design
     (Design_Name : String;
      Ship        : in out Root_Ship_Type'Class)
   is
      Design : Ship_Design renames Designs.Element (Design_Name);
      Min_W, Min_H, Min_L : Integer := Integer'Last;
      Max_W, Max_H, Max_L : Integer := Integer'First;

   begin
      for Component of Design.Components loop
         declare
            use Ada.Strings.Unbounded;
            M : constant Concorde.Modules.Module_Type :=
                  Concorde.Modules.New_Module
                    (Name      => To_String (Component.Name),
                     Component => Component.Component,
                     Size      => Component.Size);
         begin
            Ship.Structure.Append
              ((M, Component.Left_Low_Aft, Component.Orientation));
            Min_W := Integer'Min (Min_W, Component.Left_Low_Aft.X);
            Max_W := Integer'Max (Max_W, Component.Left_Low_Aft.X);
            Min_H := Integer'Min (Min_H, Component.Left_Low_Aft.Y);
            Max_H := Integer'Max (Max_H, Component.Left_Low_Aft.Y);
            Min_L := Integer'Min (Min_L, Component.Left_Low_Aft.Z);
            Max_L := Integer'Max (Max_L, Component.Left_Low_Aft.Z);
         end;
      end loop;
      Ship.Size :=
        (Max_W - Min_W + 1, Max_H - Min_H + 1, Max_L - Min_L + 1);
   end Create_Ship_From_Design;

end Concorde.Ships.Designs;
