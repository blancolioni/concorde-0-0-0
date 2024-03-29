with Ada.Calendar;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with WL.Random;
with WL.String_Maps;

with Memor.Element_Vectors;

with Xi.Camera;
with Xi.Entity;
with Xi.Float_Arrays;
with Xi.Float_Images;
with Xi.Light;
with Xi.Materials.Material;
with Xi.Matrices;
with Xi.Node;
with Xi.Scene;
with Xi.Shader.Noise;
with Xi.Shapes;

with Xtk.Text.View;

with Concorde.Elementary_Functions;
with Concorde.Solar_System;

with Concorde.Stars;

with Concorde.Ships.Xi_Model;
with Concorde.Worlds.Xi_Model;

with Concorde.Xi_UI.Outliner;
with Concorde.Xi_UI.Worlds;

with Concorde.Xi_UI.Factions;
with Concorde.Xi_UI.Ships;

with Concorde.Systems.Events;
with Concorde.Ships.Vessels;

package body Concorde.Systems.Xi_Model is

   Camera_Start_Near : constant := 0.5 * Concorde.Solar_System.Earth_Orbit;
   Camera_Start_Far  : constant := 80.0 * Concorde.Solar_System.Earth_Orbit;
   Camera_Start_Fov  : constant := 80.0;

   Star_Scale : constant := 2.5E10;

--     World_Start_Fov   : constant := 60.0;
--     World_Start_Near  : constant := 0.01;
--     World_Start_Far   : constant := 45.0;

   Scene_Unit_Length : constant :=
                         Concorde.Solar_System.Earth_Orbit / 10.0;

   package Rendered_Ship_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Concorde.Ships.Xi_Model.Active_Ship,
        Concorde.Ships.Xi_Model."=");

   type Rendered_World is
      record
         World         : Concorde.Worlds.World_Type;
         Node          : Xi.Node.Xi_Node;
         Selector_Node : Xi.Node.Xi_Node;
      end record;

   package Rendered_World_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Rendered_World);

   type Root_System_Model is
     new Concorde.Xi_UI.Root_Xi_Model with
      record
         System            : Star_System_Type;
         Start_Time        : Ada.Calendar.Time;
         Worlds            : Rendered_World_Lists.List;
         Ships             : Rendered_Ship_Lists.List;
         Selected_Ship     : Concorde.Ships.Ship_Type;
         Ships_Model       : Concorde.Xi_UI.Ships.Ship_Summary_Model;
         Ships_Overlay     : Concorde.Xi_UI.Overlay_Type;
         System_Node       : Xi.Node.Xi_Node;
         View              : System_Model_View;
         Log               : Xtk.Text.View.Xtk_Text_View;
         Logged_Ship       : Concorde.Ships.Ship_Type;
         Logged_World      : Concorde.Worlds.World_Type;
         Arrival_Handler   : access
           Concorde.Objects.Object_Handler_Interface'Class;
         Departure_Handler : access
           Concorde.Objects.Object_Handler_Interface'Class;
      end record;

   overriding procedure Transit_To_Object
     (Model         : in out Root_System_Model;
      Target_Object : not null access constant
        Concorde.Objects.Root_Object_Type'Class)
   is null;

   overriding function Base_Movement
     (Model : Root_System_Model)
      return Xi.Xi_Float
   is (Scene_Unit_Length);

   overriding procedure On_Frame_Start
     (Model      : in out Root_System_Model;
      Time_Delta : Duration);

   function Ship_Record
     (Model : Root_System_Model'Class;
      Ship  : Concorde.Ships.Ship_Type)
      return Concorde.Ships.Xi_Model.Active_Ship
     with Unreferenced;

   type System_Model_Access is access all Root_System_Model'Class;

   package System_Model_Table is
     new WL.String_Maps (System_Model_Access);

   System_Models : System_Model_Table.Map;

   package System_Scene_Vectors is
     new Memor.Element_Vectors
       (Index_Type    => Concorde.Systems.Root_Star_System_Type,
        Element_Type  => Xi.Scene.Xi_Scene,
        Default_Value => null,
        "="           => Xi.Scene."=");

   --  Created_Scenes : System_Scene_Vectors.Vector;

   function System_Scene
     (Model     : System_Model_Access;
      System    : Concorde.Systems.Star_System_Type;
      Time      : Concorde.Calendar.Time;
      View      : System_Model_View;
      Viewport  : Xi.Viewport.Xi_Viewport)
      return Xi.Scene.Xi_Scene;

   type Ship_Departure_Handler is
     new Concorde.Systems.Events.Ship_System_Event_Handler with
      record
         Model : System_Model_Access;
      end record;

   overriding procedure On_Ship_Event
     (Handler : Ship_Departure_Handler;
      Time    : Concorde.Calendar.Time;
      System  : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Ship    : not null access constant
        Concorde.Ships.Root_Ship_Type'Class);

   type Ship_Arrival_Handler is
     new Concorde.Systems.Events.Ship_System_Event_Handler with
      record
         Model : System_Model_Access;
      end record;

   overriding procedure On_Ship_Event
     (Handler : Ship_Arrival_Handler;
      Time    : Concorde.Calendar.Time;
      System  : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Ship    : not null access constant
        Concorde.Ships.Root_Ship_Type'Class);

   type System_Model_Selector is
     abstract new Concorde.Xi_UI.Select_Handler_Interface with
      record
         Model : System_Model_Access;
      end record;

   type System_Ship_Selector is
     new System_Model_Selector with
      record
         Ship : Concorde.Ships.Ship_Type;
      end record;

   overriding procedure On_Select
     (Handler : System_Ship_Selector);

   type System_World_Selector is
     new System_Model_Selector with
      record
         World : Concorde.Worlds.World_Type;
      end record;

   overriding procedure On_Select
     (Handler : System_World_Selector)
   is null;

   --------------------
   -- On_Frame_Start --
   --------------------

   overriding procedure On_Frame_Start
     (Model      : in out Root_System_Model;
      Time_Delta : Duration)
   is
   begin
      Concorde.Xi_UI.Root_Xi_Model (Model).On_Frame_Start (Time_Delta);

      case Model.View is
         when Accurate =>
            for Rendered_World of Model.Worlds loop
               Rendered_World.Node.Set_Position
                 (Xi.Float_Arrays.Real_Vector
                    (Rendered_World.World.System_Relative_Position
                         (Concorde.Calendar.Clock)));
               Rendered_World.Selector_Node.Set_Position
                 (Xi.Float_Arrays.Real_Vector
                    (Rendered_World.World.System_Relative_Position
                         (Concorde.Calendar.Clock)));
            end loop;

            for Rendered_Ship of Model.Ships loop
               Concorde.Ships.Xi_Model.Update_Ship_Position
                 (Rendered_Ship, Concorde.Calendar.Clock, (0.0, 0.0, 0.0),
                  Model.Scene.Active_Camera, False);
            end loop;

         when Schematic =>
            for Rendered_World of Model.Worlds loop
               declare
                  use Ada.Calendar;
                  Elapsed : constant Duration :=
                              Clock - Model.Start_Time;
                  Rot     : constant Real :=
                              Real (Elapsed) / 60.0
                              * Rendered_World.World.Day_Length
                              / 84_600.0
                                * 360.0;
               begin
                  Rendered_World.Node.Set_Orientation
                    (Rot, 0.0, 1.0, 0.0);
               end;
            end loop;
      end case;

      if False then
         Model.Log.Text_Buffer.Set_Text
           (Model.Logged_World.Name & ": "
            & Concorde.Locations.Long_Name
              (Model.Logged_World.Location_At (Concorde.Calendar.Clock))
            & Character'Val (10)
            & Model.Logged_Ship.Name & ": "
            & Concorde.Locations.Long_Name
              (Model.Logged_Ship.Location_At (Concorde.Calendar.Clock)));
      end if;

   end On_Frame_Start;

   ---------------
   -- On_Select --
   ---------------

   overriding procedure On_Select
     (Handler : System_Ship_Selector)
   is
   begin
      null;
   end On_Select;

   -------------------
   -- On_Ship_Event --
   -------------------

   overriding procedure On_Ship_Event
     (Handler : Ship_Departure_Handler;
      Time    : Concorde.Calendar.Time;
      System  : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Ship    : not null access constant
        Concorde.Ships.Root_Ship_Type'Class)
   is
      pragma Unreferenced (Time);
      use Rendered_Ship_Lists;
      use type Concorde.Ships.Ship_Type;
      Position : Cursor := Handler.Model.Ships.First;
   begin
      Concorde.Ships.Xi_Model.Deactivate_Ship
        (Concorde.Ships.Vessels.Vessel_Type (Ship));
      Ada.Text_IO.Put_Line
        (System.Name & ": " & Ship.Name & " enters hyperspace");

      Concorde.Xi_UI.Outliner.Remove_Item
        ("outliner-ships", Ship.Identifier);

      Handler.Model.Ships_Model.Remove_Ship (Ship);

      while Has_Element (Position) loop
         if Ship = Concorde.Ships.Xi_Model.Get_Ship (Element (Position)) then
            Handler.Model.Ships.Delete (Position);
            return;
         end if;
         Next (Position);
      end loop;
      raise Constraint_Error with Ship.Name & ": not found in system "
        & System.Name;
   end On_Ship_Event;

   -------------------
   -- On_Ship_Event --
   -------------------

   overriding procedure On_Ship_Event
     (Handler : Ship_Arrival_Handler;
      Time    : Concorde.Calendar.Time;
      System  : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Ship    : not null access constant
        Concorde.Ships.Root_Ship_Type'Class)
   is
      use Xi;
      Pos           : constant Concorde.Vectors.Vector_3 :=
                        Ship.System_Relative_Position (Time);
      Selector      : constant Concorde.Xi_UI.Select_Handler :=
                        new System_Ship_Selector'
                          (Model => Handler.Model,
                           Ship  => Concorde.Ships.Ship_Type (Ship));
      Selector_Node : constant Xi.Node.Xi_Node :=
                        Concorde.Xi_UI.Selector_With_Text
                          (Handler.Model.System_Node, Ship.Name,
                           Xi_Float (Pos (1)),
                           Xi_Float (Pos (2)),
                           Xi_Float (Pos (3)),
                           Selector);
   begin
      Ada.Text_IO.Put_Line
        (System.Name & ": " & Ship.Name & " exits hyperspace");

      declare
         use Concorde.Xi_UI.Outliner;
      begin
         Add_Item
           (Category => "outliner-ships",
            Identity => Ship.Identifier,
            Element  => Text_Element (Ship.Name),
            Tooltip  => No_Elements);
      end;

      Handler.Model.Ships_Model.Add_Ship (Ship);

      Handler.Model.Ships.Append
        (Concorde.Ships.Xi_Model.Activate_Ship
           (Ship     => Concorde.Ships.Vessels.Vessel_Type (Ship),
            Time     => Time,
            Scene    => Handler.Model.Scene,
            Primary  => Handler.Model.System_Node,
            Selector => Selector_Node));
   end On_Ship_Event;

   -----------------
   -- Ship_Record --
   -----------------

   function Ship_Record
     (Model : Root_System_Model'Class;
      Ship  : Concorde.Ships.Ship_Type)
      return Concorde.Ships.Xi_Model.Active_Ship
   is
      pragma Unreferenced (Model);
   begin
      return Concorde.Ships.Xi_Model.Get_Active_Ship
        (Concorde.Ships.Vessels.Vessel_Type (Ship));
   end Ship_Record;

   ------------------
   -- System_Model --
   ------------------

   function System_Model
     (System     : Star_System_Type;
      Faction    : Concorde.Factions.Faction_Type;
      View       : System_Model_View;
      Target     : not null access
        Xi.Scene_Renderer.Xi_Scene_Renderer_Record'Class)
      return Concorde.Xi_UI.Xi_Model
   is
      Model : System_Model_Access;
   begin

      if System_Models.Contains (System.Identifier) then
         Model := System_Models.Element (System.Identifier);
         Model.Set_Renderer (Target);
      else
         Model := new Root_System_Model;
         Model.Initialize (Faction, Target);

         Model.Start_Time := Ada.Calendar.Clock;
         Model.View := View;
         Model.System := System;
         Model.Log := Concorde.Xi_UI.Main_Log_View;
         Model.Ships_Model := Concorde.Xi_UI.Ships.New_Ship_Summary;
         Model.Ships_Overlay :=
           Concorde.Xi_UI.Ships.Ships_Overlay
             (Model.Ships_Model);
         Model.Set_Scene
           (System_Scene (Model, System, Concorde.Calendar.Clock,
            View, Target.Full_Viewport));

         declare
            use Concorde.Systems.Events;
            Departing : constant Ship_System_Handler_Access :=
                          new Ship_Departure_Handler'
                            (Ship_System_Event_Handler with
                             Model => Model);
         begin
            Add_Ship_Handler
              (System,
               Signal_Ship_Departed,
               Departing);
            Model.Departure_Handler := Departing;
         end;

         declare
            use Concorde.Systems.Events;
            Arriving : constant Ship_System_Handler_Access :=
                          new Ship_Arrival_Handler'
                            (Ship_System_Event_Handler with
                             Model => Model);
         begin
            Add_Ship_Handler
              (System,
               Signal_Ship_Arrived,
               Arriving);
            Model.Arrival_Handler := Arriving;
         end;

         Model.Show_Overlay
           (Overlay =>
              Concorde.Xi_UI.Factions.Faction_Overlay
                (Faction),
            Left  => "10px",
            Top   => "60px");

         System_Models.Insert (System.Identifier, Model);
      end if;
      return Concorde.Xi_UI.Xi_Model (Model);
   end System_Model;

   ------------------
   -- System_Scene --
   ------------------

   function System_Scene
     (Model     : System_Model_Access;
      System    : Concorde.Systems.Star_System_Type;
      Time      : Concorde.Calendar.Time;
      View      : System_Model_View;
      Viewport  : Xi.Viewport.Xi_Viewport)
      return Xi.Scene.Xi_Scene
   is
      use type Xi.Xi_Float;
      Scene       : constant Xi.Scene.Xi_Scene := Xi.Scene.Create_Scene;
      Camera      : constant Xi.Camera.Xi_Camera := Scene.Active_Camera;
      System_Node : constant Xi.Node.Xi_Node :=
                      Scene.Create_Node (System.Name);
      Selector_Node : constant Xi.Node.Xi_Node :=
                        Scene.Create_Node ("selectors");

      Schematic_Offset_X : Non_Negative_Real := 0.0;
      Overlay_Left       : Natural := 20;
      Overlay_Bottom     : constant String := "40px";

      procedure Create_Node
        (Object   : not null access constant
           Star_System_Object_Interface'Class);

      procedure Create_Star
        (Star      : Concorde.Stars.Star_Type);

      procedure Create_World
        (World     : Concorde.Worlds.World_Type;
         Primary   : access Star_System_Object_Interface'Class);

      -----------------
      -- Create_Node --
      -----------------

      procedure Create_Node
        (Object   : not null access constant
           Star_System_Object_Interface'Class)
      is
      begin
         if Object.all in Concorde.Stars.Root_Star_Type'Class then
            Create_Star
              (Concorde.Stars.Star_Type (Object));
         else
            Create_World
              (Concorde.Worlds.World_Type (Object),
               Object.Primary);
         end if;
      end Create_Node;

      -----------------
      -- Create_Star --
      -----------------

      procedure Create_Star
        (Star      : Concorde.Stars.Star_Type)
      is
         use Xi;
         Star_Node : constant Xi.Node.Xi_Node :=
                       System_Node.Create_Child (Star.Name);
         Star_Entity  : constant Xi.Entity.Xi_Entity :=
                          Xi.Shapes.Icosohedral_Sphere (3);
         Light     : Xi.Light.Xi_Light;
         Palette      : Xi.Color.Xi_Color_1D_Array (1 .. 20);
         Noise_Shader : Xi.Shader.Noise.Xi_Noise_Shader;
         Material     : Xi.Materials.Material.Xi_Material;
         Color        : constant Xi.Color.Xi_Color :=
                          Star.Color;
         Low          : constant Xi.Color.Xi_Color :=
                          Xi.Color.Shade (Color, 0.5);
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

         Star_Entity.Set_Material (Material);
         Star_Node.Set_Entity (Star_Entity);

         Xi.Light.Xi_New (Light, Xi.Light.Point);
         Light.Set_Color (Star.Color);
         Light.Set_Attenuation (0.2);
         Light.Set_Ambient_Coefficient (0.005);
         Scene.Add_Light (Light);

         case View is
            when Accurate =>
               Light.Set_Position (0.0, 0.0, 0.0);
               Star_Node.Set_Position (0.0, 0.0, 0.0);
               Star_Node.Scale (Star.Radius * Star_Scale);

            when Schematic =>
               declare
                  use Concorde.Elementary_Functions;
                  R : constant Xi_Float :=
                        Sqrt (Star.Radius);
               begin
                  Star_Node.Scale (R);
                  Light.Set_Position (0.0, 0.0, 0.0);
                  Star_Node.Set_Position (-R * 1.5, 0.0, 0.0);
                  Schematic_Offset_X := 0.0;
               end;
         end case;

      end Create_Star;

      ------------------
      -- Create_World --
      ------------------

      procedure Create_World
        (World     : Concorde.Worlds.World_Type;
         Primary   : access Star_System_Object_Interface'Class)
      is
         pragma Unreferenced (Primary);
         use Xi;
         use Concorde.Elementary_Functions;
         use Concorde.Worlds;
         World_Node : constant Xi.Node.Xi_Node :=
                        System_Node.Create_Child (World.Name);
         Ships        : Concorde.Ships.Lists.List;
         R            : constant Non_Negative_Real :=
                          (case View is
                              when Accurate =>
                                 World.Radius,
                              when Schematic =>
                             (if World.Category in Jovian_World
                              then 0.5 else 1.0)
                           * Sqrt (World.Radius));

      begin

         if View = Schematic then
            if not World.Is_Moon then
               declare
                  Overlay      : constant Concorde.Xi_UI.Overlay_Type :=
                                   Concorde.Xi_UI.Worlds.World_Overlay
                                     (World);
                  Left         : constant String :=
                                   Ada.Strings.Fixed.Trim
                                     (Natural'Image (Overlay_Left),
                                      Ada.Strings.Left);
               begin
                  Model.Show_Overlay
                    (Overlay, Left => Left & "%", Bottom => Overlay_Bottom);
                  Overlay_Left := Overlay_Left + 10;
               end;
            end if;
         end if;

         Concorde.Worlds.Xi_Model.Load_World
           (World       => World,
            Parent_Node => World_Node);

         World_Node.Scale (R);

         case View is
            when Accurate =>
               World_Node.Set_Position
                 (Xi.Float_Arrays.Real_Vector
                    (World.System_Relative_Position (Time)));
            when Schematic =>
               if World.Is_Moon then
                  World_Node.Set_Visible (False);
               else
                  World_Node.Set_Position
                    (Schematic_Offset_X + 5_000.0,
                     0.0,
                     0.0);
                  Schematic_Offset_X :=
                    World_Node.Position (1) + 5_000.0;
               end if;
         end case;

         World.Log
           (Concorde.Locations.Long_Name
              (World.Location_At (Time))
            & " "
            & Xi.Float_Images.Image (World.Radius)
            & " "
            & Xi.Float_Images.Image
              (World_Node.Position_3));

--           World_Node.Set_Position
--             (X => Orbit_Radius * Xi_Float (Cos (Position)),
--              Y => 0.0,
--              Z => Orbit_Radius * Xi_Float (Sin (Position)));

--           Concorde.Worlds.Xi_Model.Load_World
--             (World       => World,
--              Parent_Node => World_Node);

         declare
            use Xi;
            Pos           : constant Concorde.Vectors.Vector_3 :=
                              Concorde.Vectors.Vector_3
                                (World_Node.Position_3);
            Selector      : constant Concorde.Xi_UI.Select_Handler :=
                              new System_World_Selector'
                                (Model => Model,
                                 World => World);
            Node          : constant Xi.Node.Xi_Node :=
                              Concorde.Xi_UI.Selector_With_Text
                                (System_Node, World.Name,
                                 Xi_Float (Pos (1)),
                                 Xi_Float (Pos (2)),
                                 Xi_Float (Pos (3)),
                                 Selector);
         begin
            Selector_Node.Append_Child (Node);
            Node.Set_Visible (View = Accurate and then not World.Is_Moon);
            Model.Worlds.Append ((World, World_Node, Node));
         end;

         declare
            use Concorde.Xi_UI.Outliner;
         begin
            Add_Item
              (Category => "outliner-worlds",
               Identity => World.Identifier,
               Element  => Text_Element (World.Name),
               Tooltip  => No_Elements);
         end;

         World.Get_Ships (Ships);

         for Ship of Ships loop

            case View is
               when Accurate =>
                  declare
                     use Xi;
                     use Concorde.Ships.Xi_Model;
                     use type Concorde.Ships.Ship_Type;
                     Pos           : constant Concorde.Vectors.Vector_3 :=
                                       Ship.System_Relative_Position (Time);
                     Selector      : constant Concorde.Xi_UI.Select_Handler :=
                                       new System_Ship_Selector'
                                         (Model => Model,
                                          Ship  => Ship);
                     Selector_Node : constant Xi.Node.Xi_Node :=
                                       Concorde.Xi_UI.Selector_With_Text
                                         (System_Node, Ship.Name,
                                          Xi_Float (Pos (1)),
                                          Xi_Float (Pos (2)),
                                          Xi_Float (Pos (3)),
                                          Selector);
                     Rec           : constant Active_Ship :=
                                       Activate_Ship
                                         (Concorde.Ships.Vessels.Vessel_Type
                                            (Ship),
                                          Time,
                                          Scene, System_Node, Selector_Node);
                  begin
                     Model.Ships.Append (Rec);
                     declare
                        use Concorde.Xi_UI.Outliner;
                     begin
                        Add_Item
                          (Category => "outliner-ships",
                           Identity => Ship.Identifier,
                           Element  => Text_Element (Ship.Name),
                           Tooltip  => No_Elements);
                     end;
                     if Model.Logged_Ship = null then
                        Model.Logged_Ship := Ship;
                        Model.Logged_World := Ship.Current_World;
                     end if;
                  end;
               when Schematic =>
                  Model.Ships_Model.Add_Ship (Ship);
            end case;
         end loop;
      end Create_World;

   begin
      Model.Ships.Clear;
      for Object of System.Objects loop
         Create_Node (Object.Object);
      end loop;

      Model.System_Node := System_Node;

      case View is
         when Accurate =>
            Camera.Set_Position (0.0, 0.0, Camera_Start_Far / 20.0);
            Camera.Perspective (Camera_Start_Fov,
                                Camera_Start_Near, Camera_Start_Far);
         when Schematic =>
            Camera.Set_Position (30_000.0, 0.0, 60_000.0);
            Camera.Perspective (45.0, 30_000.0, 120_000.0);

--              Camera.Set_Position (Schematic_Offset_X / 2.0, 0.0,
--                                   Schematic_Offset_X);
--              Camera.Perspective (45.0, Schematic_Offset_X / 2.0,
--                                  Schematic_Offset_X * 2.0);
      end case;

      Camera.Set_Orientation (0.0, 0.0, 1.0, 0.0);
      Camera.Set_Viewport (Viewport);
      return Scene;
   end System_Scene;

   ----------------------
   -- Transit_To_World --
   ----------------------

--     procedure Transit_To_World
--       (World     : Concorde.Worlds.World_Type;
--        Model     : in out Concorde.Xi_UI.Root_Xi_Model'Class)
--     is
--        use Xi;
--        use type Xi.Scene.Xi_Scene;
--        use Concorde.Geometry;
--        use type Concorde.Worlds.World_Type;
--        Scene             : Xi.Scene.Xi_Scene :=
--                              Created_Scenes.Element (World.System);
--  --        System_Transition : constant Transitions.Transition_Type :=
--  --                              new Transitions.Root_Transition_Type;
--        World_Transition  : constant Transitions.Transition_Type :=
--                              new Transitions.Root_Transition_Type;
--        AU                : constant Real :=
--                              World.Semimajor_Axis
--                                / Concorde.Solar_System.Earth_Orbit;
--     begin
--        if Scene = null then
--           Scene := System_Scene (World.System, Model.Renderer.Viewport);
--           Created_Scenes.Replace_Element (World.System, Scene);
--        end if;
--
--        Scene.Active_Camera.Set_Viewport (Model.Renderer.Viewport);
--
--        declare
--           use Xi.Float_Arrays;
--           Orbital_Offset : constant Radians :=
--                              World.Orbit_Progress;
--           Position : constant Xi.Matrices.Vector_3 :=
--                        (AU * Cos (Orbital_Offset),
--                         0.0,
--                         AU * Sin (Orbital_Offset));
--           Target_Position : constant Xi.Matrices.Vector_3 :=
--                               Position + (0.0, 0.0, 0.015);
--           Target_Orientation : constant Xi.Matrices.Matrix_3 :=
--                                  Xi.Matrices.Look_At_Matrix
--                                    (Target_Position, (0.0, 1.0, 0.0),
--                                     Position);
--           Target_Projection  : constant Xi.Matrices.Matrix_4 :=
--                                  Xi.Matrices.Perspective_Matrix
--                                    (World_Start_Fov,
--                                     Model.Renderer.Viewport.Aspect_Ratio,
--                                     World_Start_Near, World_Start_Far);
--
--        begin
--           World_Transition.Create
--             (Scene              => Scene,
--              Target_Position    => Target_Position,
--              Target_Orientation => Target_Orientation,
--              Target_Projection  => Target_Projection,
--              Acceleration       => 1.0,
--              Max_Velocity       => 3.0);
--           Model.Add_Transition (World_Transition);
--           Concorde.Worlds.Xi_Model.Transit_To_World
--             (World, Model);
--        end;
--     end Transit_To_World;

end Concorde.Systems.Xi_Model;
