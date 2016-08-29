with Lui.Colours;

with Xi.Assets;
with Xi.Color;
with Xi.Entity;
with Xi.Float_Arrays;
with Xi.Logging;
with Xi.Materials.Material;
with Xi.Matrices;
with Xi.Shapes;
with Xi.Value;

with Concorde.Transitions;

with Xi.Transition.Orientation;

package body Concorde.Ships.Xi_Model is

   procedure Create_Module_Nodes
     (Ship : Ship_Type;
      Top  : Xi.Node.Xi_Node);

   function To_Orientation_Matrix
     (Orientation : Concorde.Ships.Module_Orientation)
      return Xi.Matrices.Matrix_4;

   -------------------------
   -- Create_Module_Nodes --
   -------------------------

   procedure Create_Module_Nodes
     (Ship : Ship_Type;
      Top  : Xi.Node.Xi_Node)
   is
      use Xi;
   begin

      for Mount_Index in 1 .. Ship.Structure.Last_Index loop
         declare
            use Concorde.Components;
            Mount     : Module_Layout_Record renames
                          Ship.Structure (Mount_Index);
            Module    : constant Concorde.Modules.Module_Type := Mount.Module;
            Component : constant Component_Type := Module.Component;
            Lui_Color : constant Lui.Colours.Colour_Type :=
                          Component.Colour;
            Colour    : constant Xi.Color.Xi_Color :=
                          (Xi_Unit_Float (Lui_Color.Red),
                           Xi_Unit_Float (Lui_Color.Green),
                           Xi_Unit_Float (Lui_Color.Blue),
                           Xi_Unit_Float (Lui_Color.Alpha));
            DX        : constant Xi_Non_Negative_Float :=
                          Xi_Float (Module.Size.X) / 2.0;
            DY        : constant Xi_Non_Negative_Float :=
                          Xi_Float (Module.Size.Y) / 2.0;
            DZ        : constant Xi_Non_Negative_Float :=
                          Xi_Float (Module.Size.Z) / 2.0;
            CX        : constant Xi_Float :=
                          Xi_Float (Mount.Left_Low_Aft.X) + DX;
            CY        : constant Xi_Float :=
                          Xi_Float (Mount.Left_Low_Aft.Y) + DY;
            CZ        : constant Xi_Float :=
                          Xi_Float (Mount.Left_Low_Aft.Z) + DZ;
            Node      : constant Xi.Node.Xi_Node :=
                          Top.Create_Child (Module.Name);
            Base      : constant Xi.Materials.Material.Xi_Material :=
                          Xi.Assets.Material ("Xi.Solid_Lit_Color");
            Material  : constant Xi.Materials.Material.Xi_Material :=
                          Base.Instantiate;
         begin
            Material.Set_Parameter_Value
              ("color", Xi.Value.Color_Value (Colour));

            Node.Set_Orientation_4
              (To_Orientation_Matrix (Mount.Orientation));

            Node.Set_Position (CX, CY, CZ);

            case Component.Shape is
               when Concorde.Components.Sphere =>
                  Node.Set_Entity (Xi.Shapes.Icosohedral_Sphere (2));
                  Node.Scale (DX, DY, DZ);

               when Concorde.Components.Cylinder =>
                  Node.Set_Entity
                    (Xi.Shapes.Quadric_Cylinder
                       (Slices => 16,
                        Stacks => Module.Size.Z));
                  Node.Scale (DX, DY, DZ);

               when Concorde.Components.Cone =>
                  Node.Set_Entity
                    (Xi.Shapes.Quadric_Cone
                       (Slices => 16,
                        Stacks => Module.Size.Z));
                  Node.Scale (DX, DY, DZ * 2.0);

               when Concorde.Components.Conical_Frustum =>
                  Node.Set_Entity
                    (Xi.Shapes.Quadric_Conical_Frustum
                       (Slices       => 16,
                        Stacks       => Module.Size.Z,
                        Radius_Ratio => 0.5));
                  Node.Scale (DX, DY, DZ);

               when Rectangular_Prism | Hexagonal_Prism | Cube =>
                  Node.Set_Entity
                    (Xi.Shapes.Cube);
                  Node.Scale (DX, DY, DZ);
            end case;

            Node.Entity.Set_Material (Material);
         end;
      end loop;
   end Create_Module_Nodes;

   ----------------------
   -- Create_Ship_Node --
   ----------------------

   procedure Create_Ship_Node
     (Ship    : Ship_Type;
      Scene   : Xi.Scene.Xi_Scene;
      Primary : Xi.Node.Xi_Node)
   is
      use Xi;
      use Xi.Float_Arrays;
      use type Xi.Entity.Xi_Entity;
      Ship_Position : constant Newton.Vector_3 :=
                        Ship.Primary_Relative_Position;
      Node_Position : constant Xi.Matrices.Vector_3 :=
                        (Xi_Float (Ship_Position (1)),
                         Xi_Float (Ship_Position (2)),
                         Xi_Float (Ship_Position (3)));
      Node          : constant Xi.Node.Xi_Node :=
                        Primary.Create_Child (Ship.Name);
   begin
      Xi.Logging.Put ("created ship " & Ship.Name & " at ");
      Xi.Logging.Put (Node_Position);
      Xi.Logging.New_Line;

--      Node.Rotate (90.0, 0.0, 1.0, 0.0);
      Node.Set_Position (Node_Position & 1.0);

      Create_Module_Nodes (Ship, Node);

      declare
         use Xi.Transition.Orientation;
         Rotate : constant Xi_Orientation_Transition :=
                    New_Orientation_Transition
                      (Node, 20.0, 360.0, 0.0, 0.0, 1.0, Cyclic => True);
      begin
         Rotate.Start;
         Scene.Add_Transition (Rotate);
      end;

   end Create_Ship_Node;

   ---------------------------
   -- To_Orientation_Matrix --
   ---------------------------

   function To_Orientation_Matrix
     (Orientation : Concorde.Ships.Module_Orientation)
      return Xi.Matrices.Matrix_4
   is
      use Xi;
      Result : Xi.Matrices.Matrix_4 :=
                 Xi.Float_Arrays.Unit_Matrix (4);
   begin
      case Orientation.Axis is
         when X_Axis =>
            Result (1, 1) := 0.0;
            Result (3, 3) := 0.0;
            if Orientation.Forward then
               Result (1, 3) := -1.0;
               Result (3, 1) := 1.0;
            else
               Result (1, 3) := 1.0;
               Result (3, 1) := -1.0;
            end if;
         when Y_Axis =>
            Result (2, 2) := 0.0;
            Result (3, 3) := 0.0;
            if Orientation.Forward then
               Result (3, 2) := -1.0;
               Result (2, 3) := 1.0;
            else
               Result (3, 2) := 1.0;
               Result (2, 3) := -1.0;
            end if;
         when Z_Axis =>
            if Orientation.Forward then
               Result (2, 2) := 1.0;
               Result (3, 3) := 1.0;
            else
               Result (2, 2) := -1.0;
               Result (3, 3) := -1.0;
            end if;
      end case;
      return Result;
   end To_Orientation_Matrix;

   ---------------------
   -- Transit_To_Ship --
   ---------------------

   procedure Transit_To_Ship
     (Ship   : Ship_Type;
      Model  : in out Concorde.Xi_UI.Root_Xi_Model'Class)
   is
      use Xi;
      Ship_Transition    : constant Concorde.Transitions.Transition_Type :=
                             new Concorde.Transitions.Root_Transition_Type;
      Ship_Position      : constant Newton.Vector_3 :=
                             Ship.Primary_Relative_Position;
      Target_Position    : constant Xi.Matrices.Vector_3 :=
                             (Xi_Float (Ship_Position (1) - 50.0),
                              Xi_Float (Ship_Position (2)),
                              Xi_Float (Ship_Position (3)));
      Target_Orientation : constant Xi.Matrices.Matrix_3 :=
                             Xi.Matrices.Rotation_Matrix
                               (-90.0, (0.0, 1.0, 0.0));
   begin
      Concorde.Xi_UI.Transit_To_Object
        (Model, Ship.Orbiting);
      Ship_Transition.Create
        (Target_Position    => Target_Position,
         Target_Orientation => Target_Orientation,
         Acceleration       => 1.0e5,
         Max_Velocity       => 5.0e5);
      Model.Add_Transition (Ship_Transition);
   end Transit_To_Ship;

end Concorde.Ships.Xi_Model;
