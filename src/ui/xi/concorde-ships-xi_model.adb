--  with Lui.Colours;

with Memor.Element_Vectors;

with Xi.Assets;
--  with Xi.Color;
with Xi.Entity;
with Xi.Float_Arrays;
with Xi.Logging;
with Xi.Materials.Material;
with Xi.Shapes;
with Xi.Value;

with Concorde.Ships.Modules;
with Concorde.Ships.Modules.Xi_Model;

with Concorde.Transitions;
with Concorde.Vectors;

with Xi.Transition.Orientation;

package body Concorde.Ships.Xi_Model is

   Ship_Render_Limit : constant Xi.Xi_Float := 1.0e4;

   type Active_Ship_Record is
      record
         Ship         : access constant
           Concorde.Ships.Vessels.Root_Vessel_Type'Class;
         Primary_Node : Xi.Node.Xi_Node;
         Holder_Node  : Xi.Node.Xi_Node;
         Ship_Node    : Xi.Node.Xi_Node;
         Radar_Node   : Xi.Node.Xi_Node;
         Local_Camera : Xi.Camera.Xi_Camera;
         Selector     : Xi.Node.Xi_Node;
      end record;

   package Active_Ship_Vectors is
     new Memor.Element_Vectors
       (Concorde.Ships.Vessels.Root_Vessel_Type, Active_Ship, null);

   Active_Ship_Vector : Active_Ship_Vectors.Vector;

   procedure Create_Module_Nodes
     (Ship : not null access constant
        Concorde.Ships.Vessels.Root_Vessel_Type'Class;
      Top  : Xi.Node.Xi_Node);

   function Ship_Node_Identity
     (Ship : not null access constant
        Concorde.Ships.Vessels.Root_Vessel_Type'Class)
      return String
   is ("[ship]" & Ship.Identifier);

   Radar_Ship_Entity : Xi.Entity.Xi_Entity;

   -------------------
   -- Activate_Ship --
   -------------------

   function Activate_Ship
     (Ship     : not null access constant
        Concorde.Ships.Vessels.Root_Vessel_Type'Class;
      Time     : Concorde.Calendar.Time;
      Scene    : Xi.Scene.Xi_Scene;
      Primary  : Xi.Node.Xi_Node;
      Selector : Xi.Node.Xi_Node)
      return Active_Ship
   is
      use Xi;
      use Xi.Float_Arrays;
      use type Xi.Entity.Xi_Entity;
      use type Xi.Node.Xi_Node;
      Ship_Position : constant Concorde.Vectors.Vector_3 :=
                        Ship.System_Relative_Position (Time);
      Node_Position : constant Xi.Matrices.Vector_3 :=
                        (Ship_Position (1),
                         Ship_Position (2),
                         Ship_Position (3));
      Node_Identity : constant String := Ship_Node_Identity (Ship);
      Holder_Node   : Xi.Node.Xi_Node :=
                        Scene.Get_Node (Node_Identity);
      Ship_Node     : Xi.Node.Xi_Node;
      Radar_Node    : Xi.Node.Xi_Node;
      Camera : Xi.Camera.Xi_Camera;
      Active : Active_Ship := Active_Ship_Vector.Element (Ship);

   begin
      if Holder_Node = null then
         Holder_Node := Primary.Create_Child (Node_Identity);
         Ship_Node := Holder_Node.Create_Child;
         Radar_Node := Holder_Node.Create_Child;
      end if;

      if Radar_Ship_Entity = null then
         Radar_Ship_Entity :=
           Xi.Shapes.Icosohedral_Sphere (2);
         declare
            Material : constant Xi.Materials.Material.Xi_Material :=
                         Xi.Assets.Material ("Xi/Solid_Color")
                         .Instantiate;
         begin
            Material.Set_Parameter_Value
              ("color", Xi.Value.Color_Value (0.1, 0.8, 0.1, 1.0));
            Radar_Ship_Entity.Set_Material (Material);
         end;
      end if;

      Radar_Node.Set_Entity (Radar_Ship_Entity);

      Ship.Log_Movement
        ("activated at "
         & Concorde.Locations.Long_Name
           (Ship.Location_At (Time)));

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
             (Ship, Primary, Holder_Node, Ship_Node, Radar_Node,
              Camera,
              Selector);
--                Concorde.Scripts.Null_Script);
         Active_Ship_Vector.Replace_Element (Ship, Active);
      else
         Active.Primary_Node := Primary;
         Active.Holder_Node := Holder_Node;
         Active.Ship_Node := Ship_Node;
         Active.Radar_Node := Radar_Node;
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
     (Ship : not null access constant
        Concorde.Ships.Vessels.Root_Vessel_Type'Class;
      Top  : Xi.Node.Xi_Node)
   is
      use Xi;
   begin

      for Mount_Index in 1 .. Ship.Get_Design.Get_Module_Count loop
         declare
            Module : constant Concorde.Ships.Modules.Module_Type :=
                       Ship.Get_Design.Get_Module (Mount_Index);
            Node      : constant Xi.Node.Xi_Node :=
                          Concorde.Ships.Modules.Xi_Model.Create_Module_Node
                            (Module, Top);
         begin
            pragma Unreferenced (Node);
         end;
      end loop;
   end Create_Module_Nodes;

   ---------------------
   -- Deactivate_Ship --
   ---------------------

   procedure Deactivate_Ship
     (Ship     : not null access constant
        Concorde.Ships.Vessels.Root_Vessel_Type'Class)
   is
      Active : constant Active_Ship := Active_Ship_Vector.Element (Ship);
      Holder : constant Xi.Node.Xi_Node :=
                 (if Active = null then null else Active.Primary_Node);
   begin
      if Active /= null then
         Ship.Log_Movement
           ("deactivated at "
            & Concorde.Locations.Long_Name
              (Ship.Location_At (Concorde.Calendar.Clock)));

         Holder.Delete_Child (Active.Holder_Node);
         if False then
            Holder.Delete_Child (Active.Selector);
         end if;
         Active_Ship_Vector.Replace_Element (Ship, Active);
      end if;
   end Deactivate_Ship;

   ---------------------
   -- Get_Active_Ship --
   ---------------------

   function Get_Active_Ship
     (Ship : not null access constant
        Concorde.Ships.Vessels.Root_Vessel_Type'Class)
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
      return Ship_Type (Active.Ship);
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

--     function To_Orientation_Matrix
--       (Orientation : Concorde.Ships.Module_Orientation)
--        return Xi.Matrices.Matrix_4
--     is
--        use Xi;
--        Result : Xi.Matrices.Matrix_4 :=
--                   Xi.Float_Arrays.Unit_Matrix (4);
--     begin
--        case Orientation.Axis is
--           when X_Axis =>
--              Result (1, 1) := 0.0;
--              Result (3, 3) := 0.0;
--              if Orientation.Forward then
--                 Result (1, 3) := -1.0;
--                 Result (3, 1) := 1.0;
--              else
--                 Result (1, 3) := 1.0;
--                 Result (3, 1) := -1.0;
--              end if;
--           when Y_Axis =>
--              Result (2, 2) := 0.0;
--              Result (3, 3) := 0.0;
--              if Orientation.Forward then
--                 Result (3, 2) := -1.0;
--                 Result (2, 3) := 1.0;
--              else
--                 Result (3, 2) := 1.0;
--                 Result (2, 3) := -1.0;
--              end if;
--           when Z_Axis =>
--              if Orientation.Forward then
--                 Result (2, 2) := 1.0;
--                 Result (3, 3) := 1.0;
--              else
--                 Result (2, 2) := -1.0;
--                 Result (3, 3) := -1.0;
--              end if;
--        end case;
--        return Result;
--     end To_Orientation_Matrix;

   ---------------------
   -- Transit_To_Ship --
   ---------------------

   procedure Transit_To_Ship
     (Ship  : not null access constant
        Concorde.Ships.Vessels.Root_Vessel_Type'Class;
      Start : Concorde.Calendar.Time;
      Model : in out Concorde.Xi_UI.Root_Xi_Model'Class)
   is
      use Xi;
      Ship_Transition    : constant Concorde.Transitions.Transition_Type :=
                             new Concorde.Transitions.Root_Transition_Type;
      Ship_Position      : constant Concorde.Vectors.Vector_3 :=
                             Ship.Primary_Relative_Position (Start);
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
     (Ship          : Active_Ship;
      Time          : Concorde.Calendar.Time;
      Relative_To   : Xi.Matrices.Vector_3;
      Camera        : Xi.Camera.Xi_Camera;
      Show_Selector : Boolean)
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

         Ship_Position : constant Concorde.Vectors.Vector_3 :=
                           Concorde.Locations.System_Relative_Position
                             (Ship.Ship.Location_At (Time), Time);
         Node_Position : constant Xi.Matrices.Vector_3 :=
                           Xi.Matrices.Vector_3 (Ship_Position) - Relative_To;
         D             : constant Xi_Non_Negative_Float :=
                           abs (Camera.Position_3
                                - Node_Position);
      begin
         Ship.Holder_Node.Set_Position (Node_Position);
         if D < Ship_Render_Limit then
            Ship.Ship_Node.Set_Visible (True);
            Ship.Radar_Node.Set_Visible (False);
         else
            Ship.Ship_Node.Set_Visible (False);
            Ship.Radar_Node.Set_Visible (True);
            Ship.Radar_Node.Scale (D / 500.0);
         end if;

         --           Ship.Selector.Set_Position (0.5 * Node_Position);
         if False then
            Ship.Selector.Set_Visible (Show_Selector);
         else
            Ship.Selector.Set_Visible (False);
         end if;

      end;
   end Update_Ship_Position;

end Concorde.Ships.Xi_Model;
