with Ada.Containers.Vectors;

with GL;

with Xi;                               use Xi;

with Xi.Assets;
with Xi.Camera;
with Xi.Color;
with Xi.Entity;
with Xi.Float_Arrays;
with Xi.Materials.Material;
with Xi.Matrices;
with Xi.Mouse;
with Xi.Node;
with Xi.Render_Target;
with Xi.Scene;
with Xi.Shader;
with Xi.Shapes;
with Xi.Texture;
with Xi.Value;

with Xi.Shader.Noise;

with WL.Random;

with Concorde.Galaxy;
with Concorde.Ships;
with Concorde.Stars;
with Concorde.Worlds;

--  with Concorde.Systems.Xi_Model;
with Concorde.Ships.Xi_Model;

with Concorde.Elementary_Functions;

with Concorde.Calendar;

with Concorde.Ships.Vessels;

package body Concorde.Xi_UI.Galaxies is

   Camera_Left      : constant := -0.1;
   Camera_Right     : constant := 0.1;
   Camera_Top       : constant := -0.1;
   Camera_Bottom    : constant := 0.1;
   Camera_Near      : constant := 0.01;
   Camera_Far       : constant := 3.0;
   Camera_Fov       : constant := 80.0;
   Focus_Fov        : constant := 40.0;
   System_Fov       : constant := 10.0;

   Star_Size : constant := 0.005;

   Initial_Transition : constant Boolean := False;

   package System_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Concorde.Systems.Star_System_Type,
        "="          => Concorde.Systems."=");

   type Root_Galaxy_Model is
     new Root_Xi_Model with
      record
         Top_Panel      : Xtk.Panel.Xtk_Panel;
         Galaxy_Node    : Xi.Node.Xi_Node;
         Highlight_Node : Xi.Node.Xi_Node;
         Active_Node    : Xi.Node.Xi_Node;
         System_Vector  : System_Vectors.Vector;
         Current_System : Concorde.Systems.Star_System_Type;
         Active_Focus   : Boolean := False;
         Focus_Length   : Xi.Xi_Float;
         Galaxy_Scene   : Xi.Scene.Xi_Scene;
         System_Scene   : Xi.Scene.Xi_Scene;
         Star_Shader    : Xi.Shader.Xi_Shader;
         Current_Near   : Xi.Xi_Float := Camera_Near;
         Current_Far    : Xi.Xi_Float := Camera_Far;
         Current_Fov    : Xi.Xi_Float := Camera_Fov;
         Transited      : Boolean := False;
         Jump_Route     : Xi.Entity.Xi_Entity;
      end record;

   overriding function Top_Panel
     (Model : Root_Galaxy_Model)
      return Xtk.Panel.Xtk_Panel
   is (Model.Top_Panel);

   overriding procedure Transit_To_Object
     (Model         : in out Root_Galaxy_Model;
      Target_Object : not null access constant
        Concorde.Objects.Root_Object_Type'Class);

   overriding procedure On_Frame_Start
     (Model      : in out Root_Galaxy_Model;
      Time_Delta : Duration);

   procedure Create_System_Node
     (Model       : in out Root_Galaxy_Model'Class;
      System      : Concorde.Systems.Star_System_Type;
      Parent_Node : Xi.Node.Xi_Node;
      Simple      : Boolean);

   Main_Model : aliased Root_Galaxy_Model;

   procedure On_Resize
     (Target : not null access Xi.Render_Target.Xi_Render_Target_Record'Class);

   procedure Update_Camera;

   ------------------------
   -- Create_System_Node --
   ------------------------

   procedure Create_System_Node
     (Model       : in out Root_Galaxy_Model'Class;
      System      : Concorde.Systems.Star_System_Type;
      Parent_Node : Xi.Node.Xi_Node;
      Simple      : Boolean)
   is
      Main_Star      : constant Concorde.Stars.Star_Type :=
                         Concorde.Stars.Star_Type
                           (System.Main_Object);
      Star_Entity    : constant Xi.Entity.Xi_Entity :=
                         Xi.Shapes.Icosohedral_Sphere (2);
      Star_Position  : constant Xi.Matrices.Vector_3 :=
                         (System.X, System.Y, System.Z);
      Node           : constant Xi.Node.Xi_Node :=
                         Parent_Node.Create_Child
                           (System.Name);

      Color          : constant Xi.Color.Xi_Color := Main_Star.Color;
      Material       : Xi.Materials.Material.Xi_Material;

      Ns             : constant Concorde.Galaxy.Array_Of_Star_Systems :=
          Concorde.Galaxy.Neighbours (System);

   begin

      for Target of Ns loop
         if Target.Index > System.Index then
            declare
               use Xi.Float_Arrays;
               use Xi.Matrices;
               P         : constant Vector_3 := Star_Position;
               Q         : constant Vector_3 :=
                             (Target.X, Target.Y, Target.Z);
               M         : constant Vector_3 :=
                             (P + Q) / 2.0;
               D         : constant Xi_Float := abs (Q - P);
               Jump_Node : constant Xi.Node.Xi_Node :=
                             Parent_Node.Create_Child
                               (System.Name & "-" & Target.Name);
            begin
               Jump_Node.Set_Position (M);
               Jump_Node.Scale (0.001, 0.001, D / 2.0);
               Jump_Node.Set_Entity (Model.Jump_Route);
               Jump_Node.Look_At (Q);
            end;
         end if;
      end loop;

      if Simple then
         declare
            Base_Material  : constant Xi.Materials.Material.Xi_Material :=
                               Xi.Assets.Material ("Xi/Solid_Lit_Color");
         begin
            Material :=
              Base_Material.Instantiate;
            Material.Set_Parameter_Value
              ("color", Xi.Value.Color_Value (Color));
         end;
      else
         declare
            Palette      : Xi.Color.Xi_Color_1D_Array (1 .. 20);
            Noise_Shader : Xi.Shader.Noise.Xi_Noise_Shader;
            Low          : constant Xi.Color.Xi_Color :=
                             Xi.Color.Shade (Color, 0.75);
            High         : constant Xi.Color.Xi_Color :=
                             Xi.Color.Shade (Color, 1.33);
         begin
            for I in Palette'Range loop
               declare
                  P : Xi.Color.Xi_Color renames Palette (I);
                  F : constant Xi_Unit_Float :=
                        Xi_Float (I) / Xi_Float (Palette'Length);
               begin
                  P :=
                    Xi.Color.Interpolate (Low, High, F);
               end;
            end loop;

            Noise_Shader :=
              Xi.Shader.Noise.Create_Noise_Shader
                (Initiator  => WL.Random.Random_Number
                   (1, 999_999),
                 Octaves    => 4.0,
                 Roughness  => 0.7,
                 Lacunarity => 40.0,
                 Palette    => Palette);
            Material := Noise_Shader.Material;
         end;
      end if;

      Node.Set_Position (Star_Position);
      Node.Scale
        (Star_Size
         * Concorde.Elementary_Functions.Sqrt (Main_Star.Solar_Masses));

      Star_Entity.Set_Material (Material);
      Node.Set_Entity (Star_Entity);

   end Create_System_Node;

   ------------------
   -- Galaxy_Model --
   ------------------

   function Galaxy_Model
     (Faction  : Concorde.Factions.Faction_Type;
      Renderer : not null access
        Xi.Scene_Renderer.Xi_Scene_Renderer_Record'Class)
      return Xi_Model
   is
      Scene   : constant Xi.Scene.Xi_Scene := Xi.Scene.Create_Scene;
      Camera  : constant Xi.Camera.Xi_Camera := Scene.Active_Camera;
      Star    : constant Xi.Entity.Xi_Entity :=
                  Xi.Shapes.Icosohedral_Sphere (4);
--                    Xi.Shapes.Quadric_Sphere (16, 16);
--                    Xi.Shapes.Square (Star_Size);
      Star_Node : constant Xi.Node.Xi_Node := Scene.Create_Node ("stars");
      Selector_Node : constant Xi.Node.Xi_Node :=
                        Scene.Create_Node ("selectors");

      function Behind
        (S1, S2 : Concorde.Systems.Star_System_Type)
         return Boolean
      is (S1.Z > S2.Z);

      package Sort is
        new System_Vectors.Generic_Sorting (Behind);

   begin

      if False then
         GL.Enable_Debug;
      end if;

      Main_Model.Initialize (Faction, Renderer);

      Main_Model.Set_Scene (Scene);
      Main_Model.Set_Renderer (Renderer);

      Main_Model.Transited := not Initial_Transition;

      Main_Model.Jump_Route :=
        Xi.Shapes.Quadric_Cylinder (8, 12);

      Star.Set_Material
        (Xi.Assets.Material ("Concorde/Galaxy/Star"));
      Main_Model.Jump_Route.Set_Material
        (Xi.Assets.Material ("Concorde/Galaxy/Jump_Route"));

      Main_Model.Galaxy_Node := Scene.Create_Node ("galaxy");
      Main_Model.Highlight_Node := Scene.Create_Node ("highlight");

      for I in 1 .. Concorde.Galaxy.System_Count loop
         declare
            System : constant Concorde.Systems.Star_System_Type :=
                       Concorde.Galaxy.Get_System (I);
         begin
            Main_Model.System_Vector.Append (System);
         end;
      end loop;

      if False then
         Sort.Sort (Main_Model.System_Vector);
      end if;

      for System of Main_Model.System_Vector loop
         Main_Model.Create_System_Node (System, Star_Node, Simple => True);

         if System.Owned then
            declare
               Selector : constant Xi.Node.Xi_Node :=
                            Selector_Node.Create_Child
                              (System.Name & " selector");
            begin
               Selector.Set_Position
                 (Xi.Xi_Float (System.X / 2.0),
                  Xi.Xi_Float (System.Y / 2.0),
                  Xi.Xi_Float (System.Z / 2.0));
               Selector.Fixed_Pixel_Size (32.0, 32.0);
               Selector.Set_Billboard (True);
               Selector.Set_Entity (Selector_Entity);
            end;
         end if;
      end loop;

      Camera.Set_Position (0.0, 0.0, 1.5);
      Camera.Set_Orientation (0.0, 0.0, 1.0, 0.0);
      Camera.Look_At (0.0, 1.0, 0.0, 0.0, 0.0, 0.0);
      Camera.Set_Viewport (Renderer.Full_Viewport);

--        Camera.Frustum
--          (Camera_Left, Camera_Right, Camera_Bottom, Camera_Top,
--           Camera_Near, Camera_Far);

--        Main_Model.Current_Fov := 45.0;
--        Main_Model.Current_Near := Camera_Near;
--        Main_Model.Current_Far := Camera_Far;
      Update_Camera;

--        Camera.Perspective
--          (Fovy         => 10.0,
--           Near         => Camera_Near,
--           Far          => Camera_Far);

      Renderer.On_Resize (On_Resize'Access);

      Main_Model.Galaxy_Scene := Scene;

      Renderer.Set_Scene (Scene);

      return Main_Model'Access;
   end Galaxy_Model;

   -------------------
   -- Frame_Started --
   -------------------

   overriding procedure On_Frame_Start
     (Model      : in out Root_Galaxy_Model;
      Time_Delta : Duration)
   is
   begin

      Root_Xi_Model (Model).On_Frame_Start (Time_Delta);

      if not Main_Model.Transited then
         declare
            use type Concorde.Ships.Ship_Type;

            First_Ship  : Concorde.Ships.Ship_Type := null;
            Second_Ship : Concorde.Ships.Ship_Type := null;

            procedure Visit_Ship
              (Ship : Concorde.Ships.Ship_Type)
              with Unreferenced;

            procedure Visit_Object
              (Object : not null access constant
                 Concorde.Systems.Star_System_Object_Interface'Class);

            ------------------
            -- Visit_Object --
            ------------------

            procedure Visit_Object
              (Object : not null access constant
                 Concorde.Systems.Star_System_Object_Interface'Class)
            is
            begin
               if Object.all in Concorde.Worlds.Root_World_Type'Class then
                  Main_Model.Transit_To_Object
                    (Concorde.Worlds.Root_World_Type'Class
                       (Object.all)'Access);
               end if;
            end Visit_Object;

            ----------------
            -- Visit_Ship --
            ----------------

            procedure Visit_Ship
              (Ship : Concorde.Ships.Ship_Type)
            is
            begin
               if First_Ship = null then
                  First_Ship := Ship;
               elsif Second_Ship = null then
                  Second_Ship := Ship;
               end if;
            end Visit_Ship;

         begin
            --            Concorde.Ships.Scan (Visit_Ship'Access);

            if False then
               Concorde.Galaxy.Get_System (1).Scan_System_Objects
                 (Visit_Object'Access);
            elsif First_Ship = null then
               Main_Model.Transit_To_Object
                 (Concorde.Galaxy.Capital_World);
            elsif True or else Second_Ship = null then
               Concorde.Ships.Xi_Model.Transit_To_Ship
                 (Concorde.Ships.Vessels.Vessel_Type (First_Ship),
                  Concorde.Calendar.Clock, Main_Model);
            else
               Concorde.Ships.Xi_Model.Transit_To_Ship
                 (Concorde.Ships.Vessels.Vessel_Type (Second_Ship),
                  Concorde.Calendar.Clock, Main_Model);
            end if;
            Main_Model.Transited := True;
         end;
      end if;

   end On_Frame_Start;

   ---------------
   -- On_Resize --
   ---------------

   procedure On_Resize
     (Target : not null access Xi.Render_Target.Xi_Render_Target_Record'Class)
   is
      Aspect_Ratio : constant Xi_Float :=
                       Target.Width / Target.Height;
      Camera       : constant Xi.Camera.Xi_Camera :=
                       Main_Model.Scene.Active_Camera;
   begin
      if False then
         Camera.Frustum
           (Camera_Left / Aspect_Ratio, Camera_Right / Aspect_Ratio,
            Camera_Bottom, Camera_Top,
            Camera_Near, Camera_Far);
      end if;
      Camera.Set_Viewport (Target.Full_Viewport);
      Update_Camera;
   end On_Resize;

   -----------------------
   -- Transit_To_Object --
   -----------------------

   overriding procedure Transit_To_Object
     (Model         : in out Root_Galaxy_Model;
      Target_Object : not null access constant
        Concorde.Objects.Root_Object_Type'Class)
   is
      use type Xi.Xi_Float;
   begin
      if Target_Object.all in Concorde.Systems.Root_Star_System_Type'Class then
         declare
            System : constant Concorde.Systems.Star_System_Type :=
                       Concorde.Systems.Star_System_Type (Target_Object);
            Position_1 : constant Xi.Matrices.Vector_3 :=
                           (Xi.Xi_Float (System.X),
                            Xi.Xi_Float (System.Y),
                            System.Z + Camera_Near * 1.2);
            Position_2   : constant Xi.Matrices.Vector_3 :=
                             (Xi.Xi_Float (System.X),
                              Xi.Xi_Float (System.Y),
                              System.Z + Camera_Near * 1.01);
            Projection_1 : constant Xi.Matrices.Matrix_4 :=
                                Xi.Matrices.Perspective_Matrix
                                  (Fovy         => Focus_Fov,
                                   Aspect_Ratio =>
                                     Model.Renderer.Viewport.Aspect_Ratio,
                                   Near         => Camera_Near,
                                   Far          => Camera_Far);
            Projection_2 : constant Xi.Matrices.Matrix_4 :=
                                Xi.Matrices.Perspective_Matrix
                                  (Fovy         => System_Fov,
                                   Aspect_Ratio =>
                                     Model.Renderer.Viewport.Aspect_Ratio,
                                   Near         => Camera_Near,
                                   Far          => Camera_Far);
            Transition_1       : constant Transitions.Transition_Type :=
                                   new Transitions.Root_Transition_Type;
            Transition_2       : constant Transitions.Transition_Type :=
                                   new Transitions.Root_Transition_Type;
         begin
            Transition_1.Create
              (Scene              => Model.Scene,
               Target_Position    => Position_1,
               Target_Orientation => Model.Scene.Active_Camera.Orientation,
               Target_Projection  => Projection_1,
               Acceleration       => 0.01,
               Max_Velocity       => 0.1);

            if True then
               Model.Add_Transition (Transition_1);
            end if;

            Transition_2.Create
              (Scene              => Model.Scene,
               Target_Position    => Position_2,
               Target_Orientation => Model.Scene.Active_Camera.Orientation,
               Target_Projection  => Projection_2,
               Transition_Time    => 3.0);
            if False then
               Model.Add_Transition (Transition_2);
            end if;

            Model.Current_System := System;
         end;
      elsif Target_Object.all in Concorde.Worlds.Root_World_Type'Class then

         declare
            use type Concorde.Systems.Star_System_Type;
            World : constant Concorde.Worlds.World_Type :=
                      Concorde.Worlds.World_Type (Target_Object);
         begin
            if Model.Current_System = null
              or else Model.Current_System.Identifier
                /= World.System.Identifier
            then
               Model.Transit_To_Object (World.System);
            end if;
--              Concorde.Systems.Xi_Model.Transit_To_World (World, Model);
         end;

      end if;
   end Transit_To_Object;

   -------------------
   -- Update_Camera --
   -------------------

   procedure Update_Camera is
   begin
      Main_Model.Scene.Active_Camera.Perspective
        (Fovy         => Main_Model.Current_Fov,
         Near         => Main_Model.Current_Near,
         Far          => Main_Model.Current_Far);
   end Update_Camera;

end Concorde.Xi_UI.Galaxies;
