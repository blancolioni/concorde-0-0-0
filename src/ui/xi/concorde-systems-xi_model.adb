with Ada.Text_IO;

with Memor.Element_Vectors;

with Xi.Assets;
with Xi.Camera;
with Xi.Light;
with Xi.Matrices;
with Xi.Node;
with Xi.Scene;
with Xi.Shapes;

--  with Concorde.Geometry;
with Concorde.Hash_Table;
with Concorde.Solar_System;

with Concorde.Stars;

with Concorde.Ships.Xi_Model;
--  with Concorde.Worlds.Xi_Model;

with Concorde.Systems.Events;

package body Concorde.Systems.Xi_Model is

   Camera_Start_Near : constant := 0.5 * Concorde.Solar_System.Earth_Orbit;
   Camera_Start_Far  : constant := 80.0 * Concorde.Solar_System.Earth_Orbit;
   Camera_Start_Fov  : constant := 80.0;

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
         Worlds            : Rendered_World_Lists.List;
         Ships             : Rendered_Ship_Lists.List;
         Selected_Ship     : Concorde.Ships.Ship_Type;
         System_Node       : Xi.Node.Xi_Node;
         Ships_Node        : Xi.Node.Xi_Node;
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
     new Concorde.Hash_Table (System_Model_Access);

   System_Models : System_Model_Table.Map;

   package System_Scene_Vectors is
     new Memor.Element_Vectors
       (Index_Type    => Concorde.Systems.Root_Star_System_Type,
        Element_Type  => Xi.Scene.Xi_Scene,
        Default_Value => null,
        "="           => Xi.Scene."=");

   --  Created_Scenes : System_Scene_Vectors.Vector;

   function System_Scene
     (Model              : System_Model_Access;
      System             : Concorde.Systems.Star_System_Type;
      Viewport           : Xi.Viewport.Xi_Viewport)
      return Xi.Scene.Xi_Scene;

   type Ship_Departure_Handler is
     new Concorde.Systems.Events.Ship_System_Event_Handler with
      record
         Model : System_Model_Access;
      end record;

   overriding procedure On_Ship_Event
     (Handler : Ship_Departure_Handler;
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
      use type Concorde.Ships.Ship_Type;
   begin
      Concorde.Xi_UI.Root_Xi_Model (Model).On_Frame_Start (Time_Delta);
      for Rendered_World of Model.Worlds loop
         Rendered_World.Node.Set_Position
           (Rendered_World.World.System_Relative_Position);
         Rendered_World.Selector_Node.Set_Position
           (Rendered_World.World.System_Relative_Position);
      end loop;

      for Rendered_Ship of Model.Ships loop
         Concorde.Ships.Xi_Model.Update_Ship_Position
           (Rendered_Ship, (0.0, 0.0, 0.0),
            Model.Scene.Active_Camera, False);
      end loop;
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
      System  : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Ship    : not null access constant
        Concorde.Ships.Root_Ship_Type'Class)
   is
      use Rendered_Ship_Lists;
      use type Concorde.Ships.Ship_Type;
      Position : Cursor := Handler.Model.Ships.First;
   begin
      Concorde.Ships.Xi_Model.Deactivate_Ship
        (Concorde.Ships.Ship_Type (Ship));
      Ada.Text_IO.Put_Line
        (System.Name & ": " & Ship.Name & " enters hyperspace");
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
      System  : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class;
      Ship    : not null access constant
        Concorde.Ships.Root_Ship_Type'Class)
   is
      use Xi;
      use Concorde.Ships.Xi_Model;
      Pos           : constant Newton.Vector_3 :=
                        Ship.Primary_Relative_Position;
      Selector      : constant Concorde.Xi_UI.Select_Handler :=
                        new System_Ship_Selector'
                          (Model => Handler.Model,
                           Ship  => Concorde.Ships.Ship_Type (Ship));
      Selector_Node : constant Xi.Node.Xi_Node :=
                        Concorde.Xi_UI.Selector_With_Text
                          (Handler.Model.Ships_Node, Ship.Name,
                           Xi_Float (Pos (1)),
                           Xi_Float (Pos (2)),
                           Xi_Float (Pos (3)),
                           Selector);
   begin
      Ada.Text_IO.Put_Line
        (System.Name & ": " & Ship.Name & " exits hyperspace");
      Handler.Model.Ships.Append
        (Concorde.Ships.Xi_Model.Activate_Ship
           (Ship     => Concorde.Ships.Ship_Type (Ship),
            Scene    => Handler.Model.Scene,
            Primary  => Handler.Model.Ships_Node,
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
      return Concorde.Ships.Xi_Model.Get_Active_Ship (Ship);
   end Ship_Record;

   ------------------
   -- System_Model --
   ------------------

   function System_Model
     (System  : Star_System_Type;
      Target  : not null access
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
         Model.Initialize (Target);
         Model.System := System;
         Model.Set_Scene (System_Scene (Model, System, Target.Full_Viewport));
         Model.System_Node :=
           Model.Scene.Get_Node (System.Identifier);

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
         end;

         System_Models.Insert (System.Identifier, Model);
      end if;
      return Concorde.Xi_UI.Xi_Model (Model);
   end System_Model;

   ------------------
   -- System_Scene --
   ------------------

   function System_Scene
     (Model              : System_Model_Access;
      System             : Concorde.Systems.Star_System_Type;
      Viewport           : Xi.Viewport.Xi_Viewport)
      return Xi.Scene.Xi_Scene
   is
      use type Xi.Xi_Float;
      Scene       : constant Xi.Scene.Xi_Scene := Xi.Scene.Create_Scene;
      Camera      : constant Xi.Camera.Xi_Camera := Scene.Active_Camera;
      System_Node : constant Xi.Node.Xi_Node :=
                      Scene.Create_Node (System.Name);
      Selector_Node : constant Xi.Node.Xi_Node :=
                        Scene.Create_Node ("selectors");

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
         Light     : Xi.Light.Xi_Light;
--           Star_Scale : constant Xi_Float :=
--                          Xi_Float (Star.Radius)
--                          * Concorde.Solar_System.Solar_Radius
--                          / Scene_Unit_Length;
      begin
         Star_Node.Set_Entity (Xi.Shapes.Icosohedral_Sphere (3));
         Star_Node.Entity.Set_Material
           (Xi.Assets.Material ("Concorde/System/Star"));
         Xi.Light.Xi_New (Light, Xi.Light.Point);
         Light.Set_Position (Star_Node.Position);
         Light.Set_Color (1.0, 1.0, 1.0, 1.0);
--           Light.Set_Attenuation (0.2);
--           Light.Set_Ambient_Coefficient (0.005);
         Scene.Add_Light (Light);

         Star_Node.Scale (Star.Radius);

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
         use Concorde.Geometry;
         --  Position : constant Radians := World.Orbit_Progress;
         World_Node : constant Xi.Node.Xi_Node :=
                        System_Node.Create_Child (World.Name);
--           Orbit_Radius : constant Xi_Float :=
--                            Xi_Float (World.Semimajor_Axis)
--                            / Concorde.Solar_System.Earth_Orbit;
--           Scale        : constant Xi_Float :=
--                            Xi_Float (World.Radius)
--                            / Scene_Unit_Length;
         Ships        : Concorde.Ships.Lists.List;
      begin
         World_Node.Scale (World.Radius);
         World_Node.Set_Position
           (World.System_Relative_Position);

--           World_Node.Set_Position
--             (X => Orbit_Radius * Xi_Float (Cos (Position)),
--              Y => 0.0,
--              Z => Orbit_Radius * Xi_Float (Sin (Position)));

--           Concorde.Worlds.Xi_Model.Load_World
--             (World       => World,
--              Parent_Node => World_Node);

         declare
            use Xi;
            use Concorde.Ships.Xi_Model;
            Pos           : constant Newton.Vector_3 :=
                              World.System_Relative_Position;
            Selector      : constant Concorde.Xi_UI.Select_Handler :=
                              new System_World_Selector'
                                (Model => Model,
                                 World => World);
            Node : constant Xi.Node.Xi_Node :=
                              Concorde.Xi_UI.Selector_With_Text
                                (System_Node, World.Name,
                                 Xi_Float (Pos (1)),
                                 Xi_Float (Pos (2)),
                                 Xi_Float (Pos (3)),
                                 Selector);
         begin
            Selector_Node.Append_Child (Node);
            Model.Worlds.Append ((World, World_Node, Node));
         end;

         World.Get_Ships (Ships);

         for Ship of Ships loop

            declare
               use Xi;
               use Concorde.Ships.Xi_Model;
               Pos           : constant Newton.Vector_3 :=
                                 Ship.System_Relative_Position;
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
                                   (Ship, Scene, System_Node, Selector_Node);
            begin
               Model.Ships.Append (Rec);
            end;
         end loop;

      end Create_World;

   begin
      Model.Ships.Clear;
      for Object of System.Objects loop
         Create_Node (Object.Object);
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
