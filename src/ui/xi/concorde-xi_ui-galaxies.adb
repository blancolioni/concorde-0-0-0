with Ada.Calendar;
with Ada.Text_IO;

with Xi;                               use Xi;

with Xi.Camera;
with Xi.Entity.Manual;
with Xi.Frame_Event;
with Xi.Keyboard;
with Xi.Main;
with Xi.Mouse;
with Xi.Node;
with Xi.Render_Operation;
with Xi.Scene;
with Xi.Shader;

with Xtk.Fixed;
with Xtk.Label;

with Concorde.Galaxy;
with Concorde.Systems;

with Concorde.Paths;
with Xi.Texture;

package body Concorde.Xi_UI.Galaxies is

   type Root_Galaxy_Model is
     new Root_Xi_Model with
      record
         Top_Panel    : Xtk.Panel.Xtk_Panel;
         FPS          : Xtk.Label.Xtk_Label;
         Render_Time  : Xtk.Label.Xtk_Label;
         Galaxy_Node  : Xi.Node.Xi_Node;
      end record;

   overriding function Top_Panel
     (Model : Root_Galaxy_Model)
      return Xtk.Panel.Xtk_Panel
   is (Model.Top_Panel);

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

   -------------------
   -- Frame_Started --
   -------------------

   overriding procedure Frame_Started
     (Listener : in out Galaxy_Frame_Listener;
      Event    : Xi.Frame_Event.Xi_Frame_Event)
   is
      use Xi;
      pragma Unreferenced (Event);
   begin
      if Xi.Mouse.Current_Mouse.Wheel_Up
        or else Xi.Keyboard.Key_Down (Xi.Keyboard.Character_Key ('e'))
      then
         Main_Model.On_Wheel_Up;
      elsif Xi.Mouse.Current_Mouse.Wheel_Down
        or else Xi.Keyboard.Key_Down (Xi.Keyboard.Character_Key ('c'))
      then
         Main_Model.On_Wheel_Down;
      elsif Xi.Keyboard.Key_Down (Xi.Keyboard.Character_Key ('a')) then
         Main_Model.Galaxy_Node.Yaw (-0.1);
      elsif Xi.Keyboard.Key_Down (Xi.Keyboard.Character_Key ('d')) then
         Main_Model.Galaxy_Node.Yaw (0.1);
      elsif Xi.Keyboard.Key_Down (Xi.Keyboard.Character_Key ('w')) then
         Main_Model.Galaxy_Node.Pitch (-0.1);
      elsif Xi.Keyboard.Key_Down (Xi.Keyboard.Character_Key ('s')) then
         Main_Model.Galaxy_Node.Pitch (0.1);
      end if;
      if Xi.Keyboard.Key_Down (Xi.Keyboard.Key_Esc) then
         Xi.Main.Leave_Main_Loop;
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
            --  Main_Model.FPS.Set_Label
            Ada.Text_IO.Put_Line
              (Lui.Approximate_Image (Lui.Real (FPS)) & " FPS");
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
      return Xi_Model
   is
      Scene    : constant Xi.Scene.Xi_Scene := Xi.Scene.Create_Scene;
      Camera   : constant Xi.Camera.Xi_Camera := Scene.Active_Camera;
      Node     : constant Xi.Node.Xi_Node := Scene.Create_Node ("galaxy");
      Entity   : Xi.Entity.Manual.Xi_Manual_Entity;
      Size     : constant := 0.01;
      Texture  : constant Xi.Texture.Xi_Texture :=
                   Xi.Texture.Create_From_Png
                     ("star",
                      Concorde.Paths.Config_File
                        ("images/stars/star.png"));
      Tex      : Xi.Shader.Xi_Shader_Value;
      Program  : constant Xi.Shader.Xi_Program := Xi.Shader.Create;
      Vert     : constant Xi.Shader.Xi_Shader :=
                   Xi.Shader.Load
                     (Concorde.Paths.Config_File
                        ("shaders/galaxy.vert"),
                      Xi.Shader.Vertex);
      Frag     : constant Xi.Shader.Xi_Shader :=
                   Xi.Shader.Load
                     (Concorde.Paths.Config_File
                        ("shaders/galaxy.frag"),
                      Xi.Shader.Fragment);
   begin
      Program.Add (Vert);
      Program.Add (Frag);
      Program.Compile;
      Scene.Set_Shader (Program);
      Tex := Program.Declare_Uniform_Value ("tex");
      Texture.Set_Uniform (Tex);

      Xi.Entity.Manual.Xi_New (Entity);
      Entity.Set_Texture (Texture);

      Entity.Begin_Operation (Xi.Render_Operation.Quad_List);
      for I in 1 .. Concorde.Galaxy.System_Count loop
         declare
            System : constant Concorde.Systems.Star_System_Type :=
                       Concorde.Galaxy.Get_System (I);
            X      : constant Xi_Float := Xi_Float (System.X);
            Y      : constant Xi_Float := Xi_Float (System.Y);
            Z      : constant Xi_Float := Xi_Float (System.Z);
         begin
            Entity.Texture_Coordinate (0.0, 0.0);
            Entity.Vertex (X - Size, Y - Size, Z);

            Entity.Texture_Coordinate (1.0, 0.0);
            Entity.Vertex (X + Size, Y - Size, Z);

            Entity.Texture_Coordinate (1.0, 1.0);
            Entity.Vertex (X + Size, Y + Size, Z);

            Entity.Texture_Coordinate (0.0, 1.0);
            Entity.Vertex (X - Size, Y + Size, Z);
         end;
      end loop;
      Entity.End_Operation;

      declare
         Node : constant Xi.Node.Xi_Node :=
                  Scene.Create_Node ("top");
      begin
         Node.Set_Entity (Entity);
      end;

      Camera.Set_Position (0.0, 0.0, 1.0);
      Camera.Set_Orientation (0.0, 0.0, 1.0, 0.0);
      Camera.Look_At (0.0, 1.0, 0.0, 0.0, 0.0, 0.0);
      Camera.Frustum (-0.1, 0.1, -0.1, 0.1, 0.05, 3.0);

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

      Main_Model.Scene := Scene;
      Main_Model.Galaxy_Node := Node;

      return Main_Model'Access;
   end Galaxy_Model;

end Concorde.Xi_UI.Galaxies;
