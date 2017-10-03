--  with Lui.Colours;

with Memor.Element_Vectors;

with Xi.Assets;
--  with Xi.Color;
with Xi.Entity;
with Xi.Float_Arrays;
with Xi.Logging;
with Xi.Materials.Material;
with Xi.Render_Operation;
with Xi.Shapes;
--  with Xi.Value;

with Concorde.Transitions;
with Concorde.Ships.Flight;

with Xi.Transition.Orientation;

with Newton.Flight;

package body Concorde.Ships.Xi_Model is

   Use_Panels : constant Boolean := False;

   type Active_Ship_Record is
      record
         Ship         : Ship_Type;
         Holder_Node  : Xi.Node.Xi_Node;
         Ship_Node    : Xi.Node.Xi_Node;
         Local_Camera : Xi.Camera.Xi_Camera;
         Newton_Ship  : access Newton.Flight.Flight_Model'Class;
         Selector     : Xi.Node.Xi_Node;
      end record;

   package Active_Ship_Vectors is
     new Memor.Element_Vectors (Root_Ship_Type, Active_Ship, null);

   Active_Ship_Vector : Active_Ship_Vectors.Vector;

   procedure Create_Module_Nodes
     (Ship : Ship_Type;
      Top  : Xi.Node.Xi_Node);

   function To_Orientation_Matrix
     (Orientation : Concorde.Ships.Module_Orientation)
      return Xi.Matrices.Matrix_4;

   function Ship_Node_Identity (Ship : Ship_Type) return String
   is ("[ship]" & Ship.Identifier);

   procedure Create_Panel
     (Entity     : Xi.Entity.Xi_Entity;
      X1, X2     : Xi.Xi_Float;
      Y1, Y2     : Xi.Xi_Float;
      Z1, Z2     : Xi.Xi_Float;
      Clockwise  : Boolean;
      Panel_Size : Xi.Xi_Non_Negative_Float := 1.0);

   -------------------
   -- Activate_Ship --
   -------------------

   function Activate_Ship
     (Ship     : Ship_Type;
      Scene    : Xi.Scene.Xi_Scene;
      Primary  : Xi.Node.Xi_Node;
      Selector : Xi.Node.Xi_Node)
      return Active_Ship
   is
      use Xi;
      use Xi.Float_Arrays;
      use type Xi.Entity.Xi_Entity;
      use type Xi.Node.Xi_Node;
      Ship_Position : constant Newton.Vector_3 :=
                        Ship.Primary_Relative_Position;
      Node_Position : constant Xi.Matrices.Vector_3 :=
                        (Ship_Position (1),
                         Ship_Position (2),
                         Ship_Position (3));
      Node_Identity : constant String := Ship_Node_Identity (Ship);
      Holder_Node   : Xi.Node.Xi_Node :=
                        Scene.Get_Node (Node_Identity);
      Ship_Node     : Xi.Node.Xi_Node;
      Camera : Xi.Camera.Xi_Camera;
      Active : Active_Ship := Active_Ship_Vector.Element (Ship);

   begin
      if Holder_Node = null then
         Holder_Node := Primary.Create_Child (Node_Identity);
         Ship_Node := Holder_Node.Create_Child;
      end if;

      Xi.Logging.Put ("created ship " & Ship.Name & " at ");
      Xi.Logging.Put (Node_Position);
      Xi.Logging.New_Line;

      --      Node.Rotate (90.0, 0.0, 1.0, 0.0);
      Holder_Node.Set_Position (Node_Position & 1.0);

      Create_Module_Nodes (Ship, Ship_Node);

      if True then
         declare
            use Xi.Transition.Orientation;
            Rotate : constant Xi_Orientation_Transition :=
                       New_Orientation_Transition
                         (Ship_Node, 120.0,
                          360.0, 0.0, 1.0, 0.0,
                          Cyclic => True);
         begin
            Scene.Add_Transition (Rotate);
         end;
      end if;

      if Active = null then
         Camera := Xi.Camera.Create;
         Holder_Node.Append_Child (Camera);
         Active :=
           new Active_Ship_Record'
             (Ship, Holder_Node, Ship_Node, Camera,
              Concorde.Ships.Flight.Create_Newtonian_Ship
                (Ship, Ship.Primary_Relative_Position, (0.0, 0.0, 0.0),
                 Newton.Matrices.Unit_Matrix (3)), Selector);
--                Concorde.Scripts.Null_Script);
         Active_Ship_Vector.Replace_Element (Ship, Active);
      else
         Active.Holder_Node := Holder_Node;
         Active.Ship_Node := Ship_Node;
         Active.Newton_Ship.Set_Location
           (Ship.Primary_Relative_Position);
         Active.Newton_Ship.Set_Velocity ((0.0, 0.0, 0.0));
         Active.Newton_Ship.Set_Orientation
           (Newton.Matrices.Unit_Matrix (3));
         Camera := Active.Local_Camera;
      end if;

      Camera.Set_Position (0.0, 0.0, 50.0);
      Camera.Look_At (0.0, 1.0, 0.0, 0.0, 0.0, 0.0);
      Camera.Perspective (45.0, 10.0, 1.0e9);

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
                  if Use_Panels then
                     declare
                        Entity : constant Xi.Entity.Xi_Entity :=
                                   Xi.Entity.Create;
                     begin

                        Entity.Begin_Operation
                          (Xi.Render_Operation.Triangle_List);

                        Create_Panel
                          (Entity, -DX, DX, -DY, DY, -DZ, -DZ, True);
                        Create_Panel
                          (Entity, -DX, DX, -DY, DY, DZ, DZ, False);
                        Create_Panel
                          (Entity, -DX, DX, -DY, -DY, -DZ, DZ, True);
                        Create_Panel
                          (Entity, -DX, DX, DY, DY, -DZ, DZ, False);
                        Create_Panel
                          (Entity, -DX, -DX, -DY, DY, -DZ, DZ, True);
                        Create_Panel
                          (Entity, DX, DX, -DY, DY, -DZ, DZ, False);

                        Entity.End_Operation;

                        Node.Set_Entity (Entity);
                     end;
                  else
                     Node.Set_Entity (Xi.Shapes.Cube);
                     Node.Scale (DX, DY, DZ);
                  end if;
            end case;

            Node.Entity.Set_Material (Material);

         end;
      end loop;
   end Create_Module_Nodes;

   ------------------
   -- Create_Panel --
   ------------------

   procedure Create_Panel
     (Entity     : Xi.Entity.Xi_Entity;
      X1, X2     : Xi.Xi_Float;
      Y1, Y2     : Xi.Xi_Float;
      Z1, Z2     : Xi.Xi_Float;
      Clockwise  : Boolean;
      Panel_Size : Xi.Xi_Non_Negative_Float := 1.0)
   is
      use Xi;

      DX : constant Xi_Float := X2 - X1;
      DY : constant Xi_Float := Y2 - Y1;
      DZ : constant Xi_Float := Z2 - Z1;

      Step_X : constant Xi_Signed_Unit_Float :=
                 (if DX < 0.0 then -1.0 elsif DX > 0.0 then 1.0 else 0.0);
      Step_Y : constant Xi_Signed_Unit_Float :=
                 (if DY < 0.0 then -1.0 elsif DY > 0.0 then 1.0 else 0.0);
      Step_Z : constant Xi_Signed_Unit_Float :=
                 (if DZ < 0.0 then -1.0 elsif DZ > 0.0 then 1.0 else 0.0);

      X  : Xi_Float := Xi_Float'Min (X1, X2);
      Y  : Xi_Float := Xi_Float'Min (Y1, Y2);
      Z  : Xi_Float := Xi_Float'Min (Z1, Z2);

      Pieces : constant Natural :=
                 Natural (Xi_Float'Max (abs DX, 1.0)
                          * Xi_Float'Max (abs DY, 1.0)
                          * Xi_Float'Max (abs DZ, 1.0)
                          / Panel_Size / Panel_Size);

      procedure Vertex (A, B : Xi_Unit_Float);

      ------------
      -- Vertex --
      ------------

      procedure Vertex (A, B : Xi_Unit_Float) is
         procedure V (X, Y, Z : Xi_Float);

         -------
         -- V --
         -------

         procedure V (X, Y, Z : Xi_Float) is
         begin
            Entity.Vertex (X, Y, Z);
         end V;

      begin
         Entity.Texture_Coordinate (A, B);

         if DX = 0.0 then
            Entity.Normal (1.0, 0.0, 0.0);
            V (X, Y + A * Panel_Size, Z + B * Panel_Size);
         elsif DY = 0.0 then
            Entity.Normal (0.0, 1.0, 0.0);
            V (X + B * Panel_Size, Y, Z + A * Panel_Size);
         else
            Entity.Normal (0.0, 0.0, 1.0);
            V (X + A * Panel_Size, Y + B * Panel_Size, Z);
         end if;
      end Vertex;

   begin

      for I in 1 .. Pieces loop
         if Clockwise then
            Vertex (0.0, 1.0);
            Vertex (1.0, 0.0);
            Vertex (0.0, 0.0);

            Vertex (0.0, 1.0);
            Vertex (1.0, 1.0);
            Vertex (1.0, 0.0);
         else
            Vertex (0.0, 0.0);
            Vertex (1.0, 0.0);
            Vertex (0.0, 1.0);

            Vertex (1.0, 0.0);
            Vertex (1.0, 1.0);
            Vertex (0.0, 1.0);
         end if;

         if Step_X = 0.0 then
            Y := Y + Step_Y;
            if Y >= Y2 then
               Y := Y1;
               Z := Z + Step_Z;
            end if;
         elsif Step_Y = 0.0 then
            Z := Z + Step_Z;
            if Z >= Z2 then
               Z := Z1;
               X := X + Step_X;
            end if;
         else
            X := X + Step_X;
            if X >= X2 then
               X := X1;
               Y := Y + Step_Y;
            end if;
         end if;

      end loop;
   end Create_Panel;

   ---------------------
   -- Deactivate_Ship --
   ---------------------

   procedure Deactivate_Ship
     (Ship    : Ship_Type)
   is
      Active : constant Active_Ship := Active_Ship_Vector.Element (Ship);
      Holder : constant Xi.Node.Xi_Node := Active.Holder_Node;
   begin
      Holder.Delete_Child (Active.Ship_Node);
      Holder.Delete_Child (Active.Selector);
      Active.Holder_Node := null;
      Active.Ship_Node := null;
      Active_Ship_Vector.Replace_Element (Ship, Active);
   end Deactivate_Ship;

   ---------------------
   -- Get_Active_Ship --
   ---------------------

   function Get_Active_Ship
     (Ship : Ship_Type)
      return Active_Ship
   is
   begin
      return Active_Ship_Vector.Element (Ship);
   end Get_Active_Ship;

   --------------
   -- Get_Ship --
   --------------

   function Get_Ship
     (Active : Active_Ship)
      return Ship_Type
   is
   begin
      return Active.Ship;
   end Get_Ship;

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

   -------------------
   -- Position_Node --
   -------------------

   function Position_Node
     (Ship : Active_Ship)
      return Xi.Node.Xi_Node
   is
   begin
      return Ship.Holder_Node;
   end Position_Node;

   ------------
   -- Script --
   ------------

--     function Script
--       (Ship : Active_Ship)
--        return Concorde.Scripts.Concorde_Script
--     is
--     begin
--        return Ship.Script;
--     end Script;

   ----------------
   -- Set_Script --
   ----------------

--     procedure Set_Script
--       (Ship   : Active_Ship;
--        Script : Concorde.Scripts.Concorde_Script)
--     is
--     begin
--        Ship.Script := Script;
--     end Set_Script;

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
                             (Ship_Position (1) - 50.0,
                              Ship_Position (2),
                              Ship_Position (3));
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

   --------------------------
   -- Update_Ship_Position --
   --------------------------

   procedure Update_Ship_Position
     (Ship        : Active_Ship;
      Relative_To : Xi.Matrices.Vector_3)
   is
      use Xi;
      use Xi.Float_Arrays;
      use type Xi.Entity.Xi_Entity;
   begin
--        Concorde.Scripts.Execute (Ship.Script);
--        if Concorde.Scripts.Complete (Ship.Script) then
--           Ship.Script := Concorde.Scripts.Null_Script;
--        end if;

      declare
         use Xi.Float_Arrays;

         Ship_Position : constant Newton.Vector_3 :=
                           Concorde.Locations.System_Relative_Position
                             (Ship.Ship.Location_At
                                (Concorde.Dates.Current_Date));
         Node_Position : constant Xi.Matrices.Vector_3 :=
                           Ship_Position - Relative_To;
      begin
         Ship.Holder_Node.Set_Position (Node_Position);
--         Ship.Selector.Set_Position (0.5 * Node_Position);
      end;
   end Update_Ship_Position;

end Concorde.Ships.Xi_Model;
