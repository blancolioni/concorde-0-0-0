--  with Lui.Colours;

with Memor.Element_Vectors;

with Xi.Assets;
--  with Xi.Color;
with Xi.Entity;
with Xi.Float_Arrays;
with Xi.Logging;
with Xi.Materials.Material;
with Xi.Matrices;
with Xi.Shapes;
--  with Xi.Value;

with Concorde.Transitions;
with Concorde.Ships.Flight;

with Xi.Transition.Orientation;

with Newton.Flight;

package body Concorde.Ships.Xi_Model is

   type Active_Ship_Record is
      record
         Ship         : Ship_Type;
         Node         : Xi.Node.Xi_Node;
         Local_Camera : Xi.Camera.Xi_Camera;
         Newton_Ship  : access Newton.Flight.Flight_Model'Class;
      end record;

   package Active_Ship_Vectors is
     new Memor.Element_Vectors (Active_Ship, null);

   Active_Ship_Vector : Active_Ship_Vectors.Vector;

   procedure Create_Module_Nodes
     (Ship : Ship_Type;
      Top  : Xi.Node.Xi_Node);

   function To_Orientation_Matrix
     (Orientation : Concorde.Ships.Module_Orientation)
      return Xi.Matrices.Matrix_4;

   function Ship_Node_Identity (Ship : Ship_Type) return String
   is ("[ship]" & Ship.Identifier);

   ----------------------
   -- Create_Ship_Node --
   ----------------------

   function Activate_Ship
     (Ship    : Ship_Type;
      Scene   : Xi.Scene.Xi_Scene;
      Primary : Xi.Node.Xi_Node)
      return Active_Ship
   is
      use Xi;
      use Xi.Float_Arrays;
      use type Xi.Entity.Xi_Entity;
      use type Xi.Node.Xi_Node;
      Ship_Position : constant Newton.Vector_3 :=
                        Ship.Primary_Relative_Position;
      Node_Position : constant Xi.Matrices.Vector_3 :=
                        (Xi_Float (Ship_Position (1)),
                         Xi_Float (Ship_Position (2)),
                         Xi_Float (Ship_Position (3)));
      Node_Identity : constant String := Ship_Node_Identity (Ship);
      Node          : Xi.Node.Xi_Node :=
                        Scene.Get_Node (Node_Identity);
      Camera : constant Xi.Camera.Xi_Camera :=
                 Xi.Camera.Create;
      Active : Active_Ship := Active_Ship_Vector.Element (Ship.Reference);

   begin
      if Node = null then
         Node := Primary.Create_Child (Node_Identity);
      end if;

      Xi.Logging.Put ("created ship " & Ship.Name & " at ");
      Xi.Logging.Put (Node_Position);
      Xi.Logging.New_Line;

      --      Node.Rotate (90.0, 0.0, 1.0, 0.0);
      Node.Set_Position (Node_Position & 1.0);
      Node.Append_Child (Camera);

      Create_Module_Nodes (Ship, Node);

      if False then
         declare
            use Xi.Transition.Orientation;
            Rotate : constant Xi_Orientation_Transition :=
                       New_Orientation_Transition
                         (Node, 120.0, 360.0, 0.0, 0.0, 1.0, Cyclic => True);
         begin
            Scene.Add_Transition (Rotate);
         end;
      end if;

      if Active = null then
         Active :=
           new Active_Ship_Record'
             (Ship, Node, Camera,
              Concorde.Ships.Flight.Create_Newtonian_Ship
                (Ship, Ship.Primary_Relative_Position, (0.0, 0.0, 0.0),
                 Newton.Matrices.Unit_Matrix (3)));
         Active_Ship_Vector.Replace_Element (Ship.Reference, Active);
      else
         Active.Node := Node;
         Active.Newton_Ship.Set_Location
           (Ship.Primary_Relative_Position);
         Active.Newton_Ship.Set_Velocity ((0.0, 0.0, 0.0));
         Active.Newton_Ship.Set_Orientation
           (Newton.Matrices.Unit_Matrix (3));
      end if;

      Active.Node := Node;

      return Active;

   end Activate_Ship;

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
--              Lui_Color : constant Lui.Colours.Colour_Type :=
--                            Component.Colour;
--              Colour    : constant Xi.Color.Xi_Color :=
--                            (Xi_Unit_Float (Lui_Color.Red),
--                             Xi_Unit_Float (Lui_Color.Green),
--                             Xi_Unit_Float (Lui_Color.Blue),
--                             Xi_Unit_Float (Lui_Color.Alpha));
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
--              Base      : constant Xi.Materials.Material.Xi_Material :=
--                            Xi.Assets.Material ("Xi.Solid_Lit_Color");
            Material  : constant Xi.Materials.Material.Xi_Material :=
                          Xi.Assets.Material
                            ("Concorde.Ships.Components."
                             & Component.Name);
         begin
--              Material.Set_Parameter_Value
--                ("color", Xi.Value.Color_Value (Colour));

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

   ---------------------
   -- Get_Active_Ship --
   ---------------------

   function Get_Active_Ship
     (Ship : Ship_Type)
      return Active_Ship
   is
   begin
      return Active_Ship_Vector.Element (Ship.Reference);
   end Get_Active_Ship;

   ------------------
   -- Local_Camera --
   ------------------

   function Local_Camera
     (Ship : Active_Ship)
      return Xi.Camera.Xi_Camera
   is
   begin
      return Ship.Local_Camera;
   end Local_Camera;

   ---------------
   -- Ship_Node --
   ---------------

   function Ship_Node
     (Ship : Active_Ship)
      return Xi.Node.Xi_Node
   is
   begin
      return Ship.Node;
   end Ship_Node;

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
         Acceleration       => 1.0e6,
         Max_Velocity       => 5.0e6);
      Model.Add_Transition (Ship_Transition);
   end Transit_To_Ship;

   -----------------
   -- Update_Ship --
   -----------------

   procedure Update_Ship
     (Ship : Active_Ship)
   is
      use Xi;
      use Xi.Float_Arrays;
      use type Xi.Entity.Xi_Entity;
      Ship_Position : constant Newton.Vector_3 :=
                        Ship.Ship.Primary_Relative_Position;
      Node_Position : constant Xi.Matrices.Vector_3 :=
                        (Xi_Float (Ship_Position (1)),
                         Xi_Float (Ship_Position (2)),
                         Xi_Float (Ship_Position (3)));
   begin
      Ship.Node.Set_Position (Node_Position);
   end Update_Ship;

end Concorde.Ships.Xi_Model;
