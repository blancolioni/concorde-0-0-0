with Ada.Text_IO;

with Memor.Element_Vectors;

with Xi.Assets;
with Xi.Camera;
with Xi.Float_Arrays;
with Xi.Light;
with Xi.Matrices;
with Xi.Node;
with Xi.Scene;
with Xi.Shapes;

with Concorde.Transitions;

with Concorde.Geometry;
with Concorde.Solar_System;

with Concorde.Stars;

with Concorde.Worlds.Db;
with Concorde.Worlds.Xi_Model;

package body Concorde.Systems.Xi_Model is

   Camera_Start_Near : constant := 0.5;
   Camera_Start_Far  : constant := 80.0;
   Camera_Start_Fov  : constant := 80.0;

   World_Start_Fov   : constant := 60.0;
   World_Start_Near  : constant := 0.01;
   World_Start_Far   : constant := 40.0;

   package System_Scene_Vectors is
     new Memor.Element_Vectors
       (Element_Type  => Xi.Scene.Xi_Scene,
        Default_Value => null,
        "="           => Xi.Scene."=");

   Created_Scenes : System_Scene_Vectors.Vector;

   function System_Scene
     (System             : Concorde.Systems.Star_System_Type;
      Viewport           : Xi.Viewport.Xi_Viewport)
      return Xi.Scene.Xi_Scene;

   ------------------
   -- System_Scene --
   ------------------

   function System_Scene
     (System             : Concorde.Systems.Star_System_Type;
      Viewport           : Xi.Viewport.Xi_Viewport)
      return Xi.Scene.Xi_Scene
   is
      use type Xi.Xi_Float;
      Scene       : constant Xi.Scene.Xi_Scene := Xi.Scene.Create_Scene;
      Camera      : constant Xi.Camera.Xi_Camera := Scene.Active_Camera;
      System_Node : constant Xi.Node.Xi_Node :=
                      Scene.Create_Node (System.Name);

      procedure Create_Node
        (Object   : Star_System_Object_Interface'Class;
         Position : Concorde.Geometry.Radians);

      procedure Create_Star
        (Star      : Concorde.Stars.Root_Star_Type'Class);

      procedure Create_World
        (World     : Concorde.Worlds.Root_World_Type'Class;
         Position  : Concorde.Geometry.Radians;
         Primary   : access Star_System_Object_Interface'Class);

      -----------------
      -- Create_Node --
      -----------------

      procedure Create_Node
        (Object   : Star_System_Object_Interface'Class;
         Position : Concorde.Geometry.Radians)
      is
      begin
         if Object in Concorde.Stars.Root_Star_Type'Class then
            Create_Star
              (Concorde.Stars.Root_Star_Type'Class (Object));
         else
            Create_World
              (Concorde.Worlds.Root_World_Type'Class (Object),
               Position,
               Object.Primary);
         end if;
      end Create_Node;

      -----------------
      -- Create_Star --
      -----------------

      procedure Create_Star
        (Star      : Concorde.Stars.Root_Star_Type'Class)
      is
         Star_Node : constant Xi.Node.Xi_Node :=
                       System_Node.Create_Child (Star.Name);
         Light     : Xi.Light.Xi_Light;
      begin
         Star_Node.Scale (0.1, 0.1, 0.1);
         Star_Node.Set_Entity (Xi.Shapes.Icosohedral_Sphere (3));
         Star_Node.Entity.Set_Material
           (Xi.Assets.Material ("Concorde/System/Star"));
         Xi.Light.Xi_New (Light, Xi.Light.Point);
         Light.Set_Position (Star_Node.Position);
         Light.Set_Color (1.0, 1.0, 1.0, 1.0);
         Light.Set_Attenuation (0.2);
         Light.Set_Ambient_Coefficient (0.005);
         Scene.Add_Light (Light);
      end Create_Star;

      ------------------
      -- Create_World --
      ------------------

      procedure Create_World
        (World     : Concorde.Worlds.Root_World_Type'Class;
         Position  : Concorde.Geometry.Radians;
         Primary   : access Star_System_Object_Interface'Class)
      is
         pragma Unreferenced (Primary);
         use Xi;
         use Concorde.Geometry;
         World_Node : constant Xi.Node.Xi_Node :=
                        System_Node.Create_Child (World.Name);
         Orbit_Radius : constant Xi_Float :=
                          Xi_Float (World.Semimajor_Axis)
                          / Concorde.Solar_System.Earth_Orbit;
         Scale        : constant Xi_Float :=
                          Xi_Float (World.Radius)
                          / Concorde.Solar_System.Earth_Orbit
                          * 2.0 * 100.0;
      begin
         World_Node.Scale (Scale, Scale, Scale);
         World_Node.Set_Position
           (X => Orbit_Radius * Xi_Float (Cos (Position)),
            Y => 0.0,
            Z => Orbit_Radius * Xi_Float (Sin (Position)));
         Concorde.Worlds.Xi_Model.Load_World
           (World       => Concorde.Worlds.Db.Reference (World),
            Parent_Node => World_Node);
      end Create_World;

   begin
      for Object of System.Objects loop
         Create_Node (Object.Object.all, Object.Start);
      end loop;

      Camera.Set_Position (0.0, 0.0, Camera_Start_Far / 5.0);
      Camera.Set_Orientation (0.0, 0.0, 1.0, 0.0);
      Camera.Set_Viewport (Viewport);
      Camera.Perspective (Camera_Start_Fov,
                          Camera_Start_Near, Camera_Start_Far);
      return Scene;
   end System_Scene;

   ----------------------
   -- Transit_To_World --
   ----------------------

   procedure Transit_To_World
     (World     : Concorde.Worlds.World_Type;
      Model     : in out Concorde.Xi_UI.Root_Xi_Model'Class)
   is
      use Xi;
      use type Xi.Scene.Xi_Scene;
      use Concorde.Geometry;
      use type Concorde.Worlds.World_Type;
      Scene             : Xi.Scene.Xi_Scene :=
                            Created_Scenes.Element (World.System.Reference);
--        System_Transition : constant Transitions.Transition_Type :=
--                              new Transitions.Root_Transition_Type;
      World_Transition  : constant Transitions.Transition_Type :=
                            new Transitions.Root_Transition_Type;
      AU                : constant Real :=
                            World.Semimajor_Axis
                              / Concorde.Solar_System.Earth_Orbit;
      Orbital_Offset    : Radians;
      Got_Orbit_Offset  : Boolean := False;
   begin
      if Scene = null then
         Scene := System_Scene (World.System, Model.Window.Viewport);
         Created_Scenes.Replace_Element (World.System.Reference, Scene);
      end if;

      Scene.Active_Camera.Set_Viewport (Model.Window.Viewport);

--        System_Transition.Scene_Transition (Scene);
--        Model.Add_Transition
--          (System_Transition);

      for Object of World.System.Objects loop
         if Object.Object.all in Concorde.Worlds.Root_World_Type'Class
           and then Concorde.Worlds.World_Type (Object.Object) = World
         then
            Orbital_Offset := Object.Start;
            Got_Orbit_Offset := True;
            exit;
         end if;
      end loop;

      if not Got_Orbit_Offset then
         Ada.Text_IO.Put_Line
           ("Warning: no orbit offset for " & World.Name);
      end if;

      declare
         use Xi.Float_Arrays;
         Position : constant Xi.Matrices.Vector_3 :=
                      (Xi_Float
                         (AU * Cos (Orbital_Offset)),
                       0.0,
                       Xi_Float
                         (AU * Sin (Orbital_Offset)));
         Target_Position : constant Xi.Matrices.Vector_3 :=
                             Position + (0.0, 0.0, 0.05);
         Target_Orientation : constant Xi.Matrices.Matrix_3 :=
                                Xi.Matrices.Look_At_Matrix
                                  (Target_Position, (0.0, 1.0, 0.0),
                                   Position);
         Target_Projection  : constant Xi.Matrices.Matrix_4 :=
                                Xi.Matrices.Perspective_Matrix
                                  (World_Start_Fov,
                                   Model.Window.Viewport.Aspect_Ratio,
                                   World_Start_Near, World_Start_Far);

      begin
         World_Transition.Create
           (Scene              => Scene,
            Target_Position    => Target_Position,
            Target_Orientation => Target_Orientation,
            Target_Projection  => Target_Projection,
            Acceleration       => 1.0,
            Max_Velocity       => 3.0);
         Model.Add_Transition (World_Transition);
      end;
   end Transit_To_World;

end Concorde.Systems.Xi_Model;
