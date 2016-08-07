with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Text_IO;

with Xi;                               use Xi;

with Xi.Camera;
with Xi.Entity;
with Xi.Frame_Event;
with Xi.Main;
with Xi.Matrices;
with Xi.Mouse;
with Xi.Node;
with Xi.Render_Target;
with Xi.Scene;
with Xi.Shader.Load;
with Xi.Shapes;
with Xi.Texture;

with Xtk.Fixed;
with Xtk.Label;

with Lui.Colours;

with Concorde.Galaxy;
with Concorde.Worlds;

with Concorde.Systems.Xi_Model;

with Concorde.Paths;

package body Concorde.Xi_UI.Galaxies is

   Show_Stars      : constant Boolean := True;
   Show_Highlights : constant Boolean := True;

   Camera_Left      : constant := -0.1;
   Camera_Right     : constant := 0.1;
   Camera_Top       : constant := -0.1;
   Camera_Bottom    : constant := 0.1;
   Camera_Near      : constant := 0.5;
   Camera_Far       : constant := 2.0;
   Camera_Fov       : constant := 80.0;
   Focus_Fov        : constant := 40.0;
   System_Fov       : constant := 10.0;

   Star_Size : constant := 0.005;

   package System_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Concorde.Systems.Star_System_Type,
        "="          => Concorde.Systems."=");

   type Root_Galaxy_Model is
     new Root_Xi_Model with
      record
         Top_Panel      : Xtk.Panel.Xtk_Panel;
         FPS            : Xtk.Label.Xtk_Label;
         Render_Time    : Xtk.Label.Xtk_Label;
         Galaxy_Node    : Xi.Node.Xi_Node;
         Highlight_Node : Xi.Node.Xi_Node;
         Active_Node    : Xi.Node.Xi_Node;
         System_Vector  : System_Vectors.Vector;
         Focus_System   : Concorde.Systems.Star_System_Type;
         Next_Focus     : Concorde.Systems.Star_System_Type;
         Active_Focus   : Boolean := False;
         Focus_Length   : Xi.Xi_Float;
         Galaxy_Scene   : Xi.Scene.Xi_Scene;
         System_Scene   : Xi.Scene.Xi_Scene;
         Star_Shader    : Xi.Shader.Xi_Shader;
         Current_Near   : Xi.Xi_Float := Camera_Near;
         Current_Far    : Xi.Xi_Float := Camera_Far;
         Current_Fov    : Xi.Xi_Float := Camera_Fov;
         Transited      : Boolean := False;
      end record;

   overriding function Top_Panel
     (Model : Root_Galaxy_Model)
      return Xtk.Panel.Xtk_Panel
   is (Model.Top_Panel);

   overriding procedure Transit_To_Object
     (Model         : in out Root_Galaxy_Model;
      Target_Object : not null access constant
        Concorde.Objects.Root_Object_Type'Class);

   Main_Model : aliased Root_Galaxy_Model;

   type Galaxy_Frame_Listener is
     new Xi.Frame_Event.Xi_Frame_Listener_Interface with
      record
         Frames : Natural := 0;
         Start  : Ada.Calendar.Time;
      end record;

   overriding procedure Frame_Started
     (Listener : in out Galaxy_Frame_Listener;
      Event    : Xi.Frame_Event.Xi_Frame_Event);

   function Node_Offset (Index : Positive) return Xi.Matrices.Vector_3;
   function Node_Colour (Index : Positive) return Xi.Matrices.Vector_3;

   procedure On_Resize
     (Target : not null access Xi.Render_Target.Xi_Render_Target_Record'Class);

   procedure Update_Camera;

   -------------------
   -- Frame_Started --
   -------------------

   overriding procedure Frame_Started
     (Listener : in out Galaxy_Frame_Listener;
      Event    : Xi.Frame_Event.Xi_Frame_Event)
   is
      use Xi;
      use type Concorde.Systems.Star_System_Type;
      pragma Unreferenced (Event);
   begin
      Main_Model.On_Frame_Start;

      if not Main_Model.Transited then
         Main_Model.Transit_To_Object
           (Concorde.Galaxy.Capital_World);  --  .Get_System (1));
         Main_Model.Transited := True;
      end if;

      Listener.Frames := Listener.Frames + 1;
      if Listener.Frames >= 100 then
         declare
            use type Ada.Calendar.Time;
            Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
            FPS : constant Float :=
                    Float (Listener.Frames) / Float (Now - Listener.Start);
         begin
            Listener.Start := Now;
            Listener.Frames := 0;
            Main_Model.FPS.Set_Label
              (Lui.Approximate_Image (Lui.Real (FPS)) & " FPS");
            if False then
               Ada.Text_IO.Put_Line
                 (Lui.Approximate_Image (Lui.Real (FPS)) & " FPS");
            end if;
         end;
      end if;

      Main_Model.Render_Time.Set_Label
        (Lui.Approximate_Image
           (Lui.Real (Main_Model.Scene.Last_Render_Time)));

   end Frame_Started;

   ------------------
   -- Galaxy_Model --
   ------------------

   function Galaxy_Model
     (Window : Xi.Render_Window.Xi_Render_Window)
      return Xi_Model
   is
      Scene       : constant Xi.Scene.Xi_Scene := Xi.Scene.Create_Scene;
      Camera      : constant Xi.Camera.Xi_Camera := Scene.Active_Camera;
      Program     : constant Xi.Shader.Xi_Shader :=
                      Xi.Shader.Load.Load ("galaxy", "galaxy");
      Highlight   : constant Xi.Shader.Xi_Shader :=
                      Xi.Shader.Load.Load ("highlight", "highlight");
      Highlight_Texture : constant Xi.Texture.Xi_Texture :=
                            Xi.Texture.Create_From_Png
                              ("highlight",
                               Concorde.Paths.Config_File
                                 ("images/stars/star-highlight.png"));

      Offset      : constant Xi.Shader.Xi_Attribute_Value :=
                      Program.Declare_Attribute_Value ("vOffset");
      Colour      : constant Xi.Shader.Xi_Attribute_Value :=
                      Program.Declare_Attribute_Value ("star_colour");
      Count       : constant Natural := Concorde.Galaxy.System_Count;

      Texture : constant Xi.Texture.Xi_Texture :=
                  Xi.Texture.Create_From_Png
                    ("star",
                     Concorde.Paths.Config_File
                       ("images/stars/star.png"));
      Star    : constant Xi.Entity.Xi_Entity :=
                  Xi.Shapes.Square (Star_Size);
      Highlight_Entity : constant Xi.Entity.Xi_Entity :=
                           Xi.Shapes.Square (Star_Size);
      Tex      : Xi.Shader.Xi_Uniform_Value;

      function Behind
        (S1, S2 : Concorde.Systems.Star_System_Type)
         return Boolean
      is (S1.Z > S2.Z);

      package Sort is
        new System_Vectors.Generic_Sorting (Behind);

   begin

      Main_Model.Scene := Scene;
      Main_Model.Window := Window;
      Main_Model.Star_Shader :=
        Xi.Shader.Load.Load ("star", "star");

      Scene.Set_Shader (Program);
      Star.Set_Texture (Texture);
      Star.Bind_Shader
        (Vertices => Program.Declare_Attribute_Value ("vPosition"),
         Textures => Program.Declare_Attribute_Value ("texture_coord"));

      Tex := Program.Declare_Uniform_Value ("tex");
      Texture.Set_Uniform (Tex);

      Highlight_Entity.Set_Texture (Highlight_Texture);
      Highlight_Texture.Set_Uniform
        (Highlight.Declare_Uniform_Value ("tex"));
      Highlight_Entity.Bind_Shader
        (Vertices => Highlight.Declare_Attribute_Value ("vPosition"),
         Textures => Highlight.Declare_Attribute_Value ("texture_coord"));

      Main_Model.Galaxy_Node := Scene.Create_Node ("galaxy");
      Main_Model.Highlight_Node := Scene.Create_Node ("highlight");
      Main_Model.Highlight_Node.Set_Shader (Highlight);

      for I in 1 .. Concorde.Galaxy.System_Count loop
         declare
            System : constant Concorde.Systems.Star_System_Type :=
                       Concorde.Galaxy.Get_System (I);
         begin
            Main_Model.System_Vector.Append (System);
            if Show_Highlights and then System.Owned then
               declare
                  Node : constant Xi.Node.Xi_Node :=
                           Main_Model.Highlight_Node.Create_Child
                             ("owned-" & System.Name);
                  Colour : constant Lui.Colours.Colour_Type :=
                             System.Owner.Colour;
               begin
                  Node.Set_Position
                    (Xi.Xi_Float (System.X),
                     Xi.Xi_Float (System.Y),
                     Xi.Xi_Float (System.Z));
                  Node.Set_Color (Xi.Xi_Unit_Float (Colour.Red),
                                  Xi.Xi_Unit_Float (Colour.Green),
                                  Xi.Xi_Unit_Float (Colour.Blue),
                                  0.5);
                  Node.Set_Entity (Highlight_Entity);
                  Node.Set_Billboard (True);
               end;
            end if;
         end;
      end loop;

      Main_Model.Next_Focus := Main_Model.System_Vector.First_Element;

      Sort.Sort (Main_Model.System_Vector);

      Camera.Set_Position (0.0, 0.0, 1.0);
      Camera.Set_Orientation (0.0, 0.0, 1.0, 0.0);
      Camera.Look_At (0.0, 1.0, 0.0, 0.0, 0.0, 0.0);

      Camera.Set_Viewport (Window.Full_Viewport);

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

      Window.On_Resize (On_Resize'Access);

      if Show_Stars then
         Main_Model.Galaxy_Node.Set_Entity (Star);
         Main_Model.Galaxy_Node.Set_Instanced (Count);
         Main_Model.Galaxy_Node.Set_Instance_Value
           (Offset, Node_Offset'Access);
         Main_Model.Galaxy_Node.Set_Instance_Value
           (Colour, Node_Colour'Access);
      end if;

      if True then
         declare
            Listener : constant Xi.Frame_Event.Xi_Frame_Listener :=
                         new Galaxy_Frame_Listener'
                           (Frames => 0,
                            Start  => Ada.Calendar.Clock);
         begin
            Xi.Main.Add_Frame_Listener (Listener);
         end;

         Xtk.Label.Xtk_New (Main_Model.FPS, "FPS");
         Main_Model.FPS.Set_Rectangle ((200.0, 20.0, 180.0, 40.0));

         Xtk.Label.Xtk_New (Main_Model.Render_Time, "Render time");
         Main_Model.Render_Time.Set_Rectangle ((20.0, 20.0, 180.0, 40.0));

         declare
            Fixed : Xtk.Fixed.Xtk_Fixed;
         begin
            Xtk.Fixed.Xi_New (Fixed);
            Fixed.Set_Rectangle ((20.0, 50.0, 400.0, 60.0));
            Fixed.Add (Main_Model.FPS);
            Fixed.Add (Main_Model.Render_Time);

            Xtk.Panel.Xtk_New
              (Panel  => Main_Model.Top_Panel,
               Region => (20.0, 50.0, 400.0, 60.0),
               Top    => Fixed);
         end;
      end if;

      Main_Model.Galaxy_Scene := Scene;

      Window.Set_Scene (Scene);

      return Main_Model'Access;
   end Galaxy_Model;

   -----------------
   -- Node_Colour --
   -----------------

   function Node_Colour (Index : Positive) return Xi.Matrices.Vector_3 is
      use type Xi.Xi_Float;
      System : constant Concorde.Systems.Star_System_Type :=
                 Main_Model.System_Vector.Element (Index);
      Colour : constant Lui.Colours.Colour_Type :=
                 System.Main_Object.Colour;
      R      : constant Xi.Xi_Unit_Float :=
                 Xi.Xi_Unit_Float (Colour.Red);
      G      : constant Xi.Xi_Unit_Float :=
                 Xi.Xi_Unit_Float (Colour.Green);
      B      : constant Xi.Xi_Unit_Float :=
                 Xi.Xi_Unit_Float (Colour.Blue);
   begin
      return (R, G, B);
   end Node_Colour;

   -----------------
   -- Node_Offset --
   -----------------

   function Node_Offset (Index : Positive) return Xi.Matrices.Vector_3 is
      use type Xi.Xi_Float;
      System : constant Concorde.Systems.Star_System_Type :=
                 Main_Model.System_Vector.Element (Index);
      X      : constant Xi.Xi_Float := Xi.Xi_Float (System.X);
      Y      : constant Xi.Xi_Float := Xi.Xi_Float (System.Y);
      Z      : constant Xi.Xi_Float := Xi.Xi_Float (System.Z);
   begin
      return (X, Y, Z);
   end Node_Offset;

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
                            Xi.Xi_Float (System.Z + Camera_Near * 1.05));
            Position_2   : constant Xi.Matrices.Vector_3 :=
                             (Xi.Xi_Float (System.X),
                              Xi.Xi_Float (System.Y),
                              Xi.Xi_Float (System.Z + Camera_Near * 1.01));
            Projection_1 : constant Xi.Matrices.Matrix_4 :=
                                Xi.Matrices.Perspective_Matrix
                                  (Fovy         => Focus_Fov,
                                   Aspect_Ratio =>
                                     Model.Window.Viewport.Aspect_Ratio,
                                   Near         => Camera_Near,
                                   Far          => Camera_Far);
            Projection_2 : constant Xi.Matrices.Matrix_4 :=
                                Xi.Matrices.Perspective_Matrix
                                  (Fovy         => System_Fov,
                                   Aspect_Ratio =>
                                     Model.Window.Viewport.Aspect_Ratio,
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
               Acceleration       => 0.05,
               Max_Velocity       => 0.1);
            Model.Add_Transition (Transition_1);
            Transition_2.Create
              (Scene              => Model.Scene,
               Target_Position    => Position_2,
               Target_Orientation => Model.Scene.Active_Camera.Orientation,
               Target_Projection  => Projection_2,
               Transition_Time    => 3.0);
            Model.Add_Transition (Transition_2);
            Model.Focus_System := System;
         end;
      elsif Target_Object.all in Concorde.Worlds.Root_World_Type'Class then

         declare
            World : constant Concorde.Worlds.World_Type :=
                      Concorde.Worlds.World_Type (Target_Object);
         begin
            Model.Transit_To_Object (World.System);
            Concorde.Systems.Xi_Model.Transit_To_World (World, Model);
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
