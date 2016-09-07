with Xi.Assets;
with Xi.Camera;
with Xi.Entity;
with Xi.Float_Arrays;
with Xi.Materials.Material;
with Xi.Materials.Pass;
with Xi.Matrices;
with Xi.Render_Operation;
with Xi.Scene;
with Xi.Shapes;
with Xi.Value;

with Xi.Transition.Container.Sequential;
with Xi.Transition.Translation;

with Lui.Colours;

with Newton;

with Concorde.Hash_Table;
--  with Concorde.Transitions;

with Concorde.Worlds.Tables;
with Concorde.Ships.Xi_Model;

--  with Concorde.Xi_UI.Colours;

--  with Concorde.Solar_System;

--  with Concorde.Money;
--  with Concorde.Quantities;

with Concorde.Empires;

package body Concorde.Worlds.Xi_Model is

   type Map_Mode_Type is (Height_Mode, Temperature_Mode);
   pragma Unreferenced (Temperature_Mode);

   Current_Map_Mode : constant Map_Mode_Type := Height_Mode;
   pragma Unreferenced (Current_Map_Mode);

   Selected_Colour : constant Lui.Colours.Colour_Type :=
                       (0.8, 0.7, 0.1, 0.5);
   pragma Unreferenced (Selected_Colour);

   Height_Material_Array                : array (Height_Range)
     of Xi.Materials.Material.Xi_Material :=
       (others => null);

   type Rendered_World_Record is
      record
         Scene   : Xi.Scene.Xi_Scene;
         World   : World_Type;
         Entity  : Xi.Entity.Xi_Entity;
      end record;

   package Rendered_World_Table is
     new Concorde.Hash_Table (Rendered_World_Record);

   Rendered_Worlds : Rendered_World_Table.Map;

--     type Rendered_Ship_Record is
--        record
--           Ship          : Concorde.Ships.Ship_Type;
--           Node          : Xi.Node.Xi_Node;
--           Follow_Camera : Xi.Camera.Xi_Camera;
--        end record;

   package Rendered_Ship_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Concorde.Ships.Xi_Model.Active_Ship,
        Concorde.Ships.Xi_Model."=");

   type Root_World_Model is
     new Concorde.Xi_UI.Root_Xi_Model with
      record
         World : World_Type;
         Ships : Rendered_Ship_Lists.List;
      end record;

   overriding procedure Transit_To_Object
     (Model         : in out Root_World_Model;
      Target_Object : not null access constant
        Concorde.Objects.Root_Object_Type'Class)
   is null;

   overriding function Base_Movement
     (Model : Root_World_Model)
      return Xi.Xi_Float
   is (10_000_000.0);

   overriding procedure On_Frame_Start
     (Model      : in out Root_World_Model;
      Time_Delta : Duration);

   function Ship_Record
     (Model : Root_World_Model'Class;
      Ship  : Concorde.Ships.Ship_Type)
      return Concorde.Ships.Xi_Model.Active_Ship;

   type World_Model_Access is access all Root_World_Model'Class;

   package World_Model_Table is
     new Concorde.Hash_Table (World_Model_Access);

   World_Models : World_Model_Table.Map;

   type World_Model_Selector is
     abstract new Concorde.Xi_UI.Select_Handler_Interface with
      record
         Model : World_Model_Access;
      end record;

   type World_Ship_Selector is
     new World_Model_Selector with
      record
         Ship : Concorde.Ships.Ship_Type;
      end record;

   overriding procedure On_Select
     (Handler : World_Ship_Selector);

   function Create_Tiles
     (World       : World_Type)
      return Xi.Entity.Xi_Entity;

   function World_Scene
     (World : World_Type;
      Model : World_Model_Access)
      return Xi.Scene.Xi_Scene;

   function World_Entity
     (World : World_Type)
      return Xi.Entity.Xi_Entity;

   function Height_Material
     (Height : Height_Range)
      return Xi.Materials.Material.Xi_Material;

   type World_Transition_Callback is
     abstract new Xi.Transition.Transition_Callback_Interface with
      record
         Model : World_Model_Access;
      end record;

   type Ship_Transition_Callback is
     new World_Transition_Callback with
      record
         Ship : Concorde.Ships.Ship_Type;
      end record;

   overriding procedure Execute
     (Callback : in out Ship_Transition_Callback);

   ------------------
   -- Create_Tiles --
   ------------------

   function Create_Tiles
     (World       : World_Type)
      return Xi.Entity.Xi_Entity
   is
      Surface : constant Concorde.Surfaces.Surface_Type :=
                  World.Surface;

      function Tile_Entity
        (Index    : Concorde.Surfaces.Surface_Tile_Index;
         Material : Xi.Materials.Material.Xi_Material)
         return Xi.Entity.Xi_Entity;

      -----------------
      -- Tile_Entity --
      -----------------

      function Tile_Entity
        (Index    : Concorde.Surfaces.Surface_Tile_Index;
         Material : Xi.Materials.Material.Xi_Material)
         return Xi.Entity.Xi_Entity
      is
         use Xi;
         use type Xi.Matrices.Vector_3;

         Boundary : constant Concorde.Surfaces.Tile_Vertex_Array :=
                      Surface.Tile_Boundary (Index);
         Result : Xi.Entity.Xi_Entity;

         function Vertex
           (V : Concorde.Surfaces.Vector_3)
            return Xi.Matrices.Vector_3
         is ((Xi_Float (V (1)),
              Xi_Float (V (2)),
              Xi_Float (V (3))));

         Normal : Xi.Matrices.Vector_3 := (others => 0.0);

      begin
         Xi.Entity.Xi_New (Result);
         Result.Set_Material (Material);

         Result.Begin_Operation (Xi.Render_Operation.Triangle_Fan);
         --  Result.Color (Concorde.Xi_UI.Colours.To_Xi_Color (Colour));

         for V of Boundary loop
            Normal := Normal + Vertex (V);
         end loop;

         Normal := Xi.Matrices.Normalise (Normal);

         for V of Boundary loop
            Result.Normal (Normal);
            Result.Color ((0.0, 0.0, 0.0, 0.0));
            Result.Vertex (Vertex (V));
         end loop;

         Result.End_Operation;

         return Result;
      end Tile_Entity;

      Result : Xi.Entity.Xi_Entity;

   begin

      Xi.Entity.Xi_New (Result);

      for I in 1 .. Surface.Tile_Count loop
         declare
            use type Concorde.Features.Feature_Type;
            use type Concorde.Terrain.Terrain_Type;
            Raw_Height  : constant Height_Range :=
                            World.Sectors (I).Height;
            Height      : constant Height_Range :=
                            (if Raw_Height < 0
                             then ((Raw_Height + 2) / 3) * 3 - 2
                             else Raw_Height);
            Material : constant Xi.Materials.Material.Xi_Material :=
                         Height_Material (Height);
         begin
            Result.Add_Child (Tile_Entity (I, Material));
         end;
      end loop;

      return Result;

   end Create_Tiles;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Callback : in out Ship_Transition_Callback)
   is
      Rec : constant Concorde.Ships.Xi_Model.Active_Ship :=
              Callback.Model.Ship_Record (Callback.Ship);
   begin
      Callback.Model.Scene.Use_Camera
        (Concorde.Ships.Xi_Model.Local_Camera (Rec));
   end Execute;

   ---------------------
   -- Height_Material --
   ---------------------

   function Height_Material
     (Height : Height_Range)
      return Xi.Materials.Material.Xi_Material
   is
      use Xi.Materials.Material;
      Base_Material : constant Xi.Materials.Material.Xi_Material :=
                        Xi.Assets.Material ("Xi/Solid_Lit_Color");

      Material      : Xi_Material renames Height_Material_Array (Height);
   begin
      if Material = null then
         Material := Base_Material.Instantiate;
         declare
            use Xi;
            Height_Colour : constant Lui.Colours.Colour_Type :=
                              Concorde.Worlds.Tables.Height_Colour
                                (Height);
         begin
            Material.Set_Parameter_Value
              (Parameter_Name => "color",
               Value          =>
                 Xi.Value.Color_Value
                   (Xi_Unit_Float (Height_Colour.Red),
                    Xi_Unit_Float (Height_Colour.Green),
                    Xi_Unit_Float (Height_Colour.Blue),
                    1.0));
         end;
      end if;

      return Material;
   end Height_Material;

   ----------------
   -- Load_World --
   ----------------

   procedure Load_World
     (World       : World_Type;
      Parent_Node : Xi.Node.Xi_Node)
   is
   begin
      Parent_Node.Set_Entity
        (World_Entity (World));
   end Load_World;

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
--        Scene : constant Xi.Scene.Xi_Scene := World_Scene (World);
--        World_Transition : constant Concorde.Transitions.Transition_Type :=
--                             new Concorde.Transitions.Root_Transition_Type;
--        Target_Position : constant Xi.Matrices.Vector_3 :=
--                            (0.0, Xi_Float (World.Radius),
--                             Xi_Float (World.Radius) + 1_000_000.0);
--     begin
--        Scene.Active_Camera.Set_Viewport (Model.Renderer.Viewport);
--        Scene.Active_Camera.Set_Position
--          (0.0, 0.0, 224398100.0);
--        Scene.Active_Camera.Perspective
--          (60.0, 10.0, 1.0e9);
--        Scene.Active_Camera.Look_At (0.0, 0.0, 0.0);
--
--        if True then
--           World_Transition.Scene_Transition (Scene);
--        else
--           World_Transition.Create
--             (Scene              => Scene,
--              Target_Position    => Target_Position,
--              Target_Orientation => Scene.Active_Camera.Orientation,
--              Acceleration       => 7.0e6,
--              Max_Velocity       => 35.0e6);
--        end if;
--
--        Model.Add_Transition (World_Transition);
--     end Transit_To_World;

   --------------------
   -- On_Frame_Start --
   --------------------

   overriding procedure On_Frame_Start
     (Model      : in out Root_World_Model;
      Time_Delta : Duration)
   is
   begin
      Concorde.Xi_UI.Root_Xi_Model (Model).On_Frame_Start (Time_Delta);
      for Rendered_Ship of Model.Ships loop
         Concorde.Ships.Xi_Model.Update_Ship
           (Rendered_Ship);
      end loop;
   end On_Frame_Start;

   ---------------
   -- On_Select --
   ---------------

   overriding procedure On_Select
     (Handler : World_Ship_Selector)
   is
      use Xi;
      use Xi.Float_Arrays;
      use Xi.Matrices;
      use Xi.Transition.Container.Sequential;
      use Concorde.Ships.Xi_Model;

      Ship_Rec      : constant Active_Ship :=
                        Handler.Model.Ship_Record (Handler.Ship);
      Camera        : constant Xi.Camera.Xi_Camera :=
                        Handler.Model.Scene.Default_Camera;
      Translation_1 : constant Xi.Transition.Xi_Transition :=
                        Xi.Transition.Translation.Translate
                          (Node            => Camera,
                           Transition_Time => 3.0,
                           Target_Node     => Ship_Node (Ship_Rec),
                           Offset          => (0.0, 0.0, 100_000.0));
      Translation_2 : constant Xi.Transition.Xi_Transition :=
                        Xi.Transition.Translation.Translate
                          (Node            => Camera,
                           Transition_Time => 3.0,
                           Target_Node     => Ship_Node (Ship_Rec),
                           Offset          => (0.0, 0.0, 10_000.0));
      Translation_3 : constant Xi.Transition.Xi_Transition :=
                        Xi.Transition.Translation.Translate
                          (Node            => Camera,
                           Transition_Time => 3.0,
                           Target_Node     => Ship_Node (Ship_Rec),
                           Offset          => (0.0, 0.0, 1_000.0));
      Translation_4 : constant Xi.Transition.Xi_Transition :=
                        Xi.Transition.Translation.Translate
                          (Node            => Camera,
                           Transition_Time => 3.0,
                           Target_Node     => Ship_Node (Ship_Rec),
                           Offset          => (0.0, 0.0, 100.0));
      Transition    : constant Xi_Sequential_Transition :=
                        New_Sequential_Transition;
      Callback      : constant Xi.Transition.Transition_Callback :=
                        new Ship_Transition_Callback'
                          (Handler.Model, Handler.Ship);

   begin
      Handler.Model.Scene.Default_Camera.Set_Position
        (Handler.Model.Scene.Active_Camera.Position);
      Handler.Model.Scene.Default_Camera.Set_Orientation
        (Handler.Model.Scene.Active_Camera.Orientation);
      Handler.Model.Scene.Use_Default_Camera;
      Transition.Append (Translation_1);
      Transition.Append (Translation_2);
      Transition.Append (Translation_3);
      Transition.Append (Translation_4);
      Transition.On_Complete (Callback);
      Handler.Model.Scene.Add_Transition (Transition);
   end On_Select;

   -----------------
   -- Ship_Record --
   -----------------

   function Ship_Record
     (Model : Root_World_Model'Class;
      Ship  : Concorde.Ships.Ship_Type)
      return Concorde.Ships.Xi_Model.Active_Ship
   is
      pragma Unreferenced (Model);
   begin
      return Concorde.Ships.Xi_Model.Get_Active_Ship (Ship);
   end Ship_Record;

   ------------------
   -- World_Entity --
   ------------------

   function World_Entity
     (World : World_Type)
      return Xi.Entity.Xi_Entity
   is
      Entity : Xi.Entity.Xi_Entity;
   begin
      World.Check_Loaded;
      if World.Category in Jovian_World then
         Entity := Xi.Shapes.Icosohedral_Sphere (3);
         Entity.Set_Material
           (Xi.Assets.Material ("Concorde/System/Gas_Giant"));
      elsif World.Is_Moon then
         Entity := Xi.Shapes.Icosohedral_Sphere (3);
         Entity.Set_Material
           (Xi.Assets.Material ("Concorde/System/Moon"));
      else
         Entity := Create_Tiles (World);
      end if;
      return Entity;
   end World_Entity;

   -----------------
   -- World_Model --
   -----------------

   function World_Model
     (World  : World_Type;
      Target     : not null access
        Xi.Scene_Renderer.Xi_Scene_Renderer_Record'Class)
      return Concorde.Xi_UI.Xi_Model
   is
      Model : World_Model_Access;
   begin
      if World_Models.Contains (World.Identifier) then
         Model := World_Models.Element (World.Identifier);
         Model.Set_Renderer (Target);
      else
         Model := new Root_World_Model;
         Model.Initialize (Target);
         Model.World := World;
         Model.Set_Scene (World_Scene (World, Model));
         Model.Scene.Active_Camera.Set_Position
           (0.0, 0.0, Xi.Xi_Float (World.Radius * 5.0));
         Model.Scene.Active_Camera.Look_At
           (0.0, 1.0, 0.0, 0.0, 0.0, 0.0);
         Model.Scene.Active_Camera.Set_Viewport (Target.Full_Viewport);
         Model.Scene.Active_Camera.Perspective
           (45.0, 10.0, 1.0e9);

         World_Models.Insert (World.Identifier, Model);
      end if;
      return Concorde.Xi_UI.Xi_Model (Model);
   end World_Model;

   -----------------
   -- World_Scene --
   -----------------

   function World_Scene
     (World : World_Type;
      Model : World_Model_Access)
      return Xi.Scene.Xi_Scene
   is
      use type Xi.Entity.Xi_Entity;
      Scene : Xi.Scene.Xi_Scene;
      Rec   : Rendered_World_Record;
   begin
      if Rendered_Worlds.Contains (World.Identifier) then
         declare
            use type Xi.Scene.Xi_Scene;
         begin
            Rec := Rendered_Worlds.Element (World.Identifier);

            if Rec.Scene /= null then
               return Rec.Scene;
            end if;
         end;
      else
         Rec := (null, World, null);
      end if;

      Scene := Xi.Scene.Create_Scene;
      Rec.Scene := Scene;

      if Rec.Entity = null then
         Rec.Entity := World_Entity (World);
      end if;

      if Rendered_Worlds.Contains (World.Identifier) then
         Rendered_Worlds.Replace
           (World.Identifier, Rec);
      else
         Rendered_Worlds.Insert
           (World.Identifier, Rec);
      end if;

      declare
         World_Node : constant Xi.Node.Xi_Node :=
                        Scene.Create_Node (World.Identifier);
         Ships_Node : constant Xi.Node.Xi_Node :=
                        Scene.Create_Node (World.Identifier & " ships");
         Moons_Node : constant Xi.Node.Xi_Node :=
                        Scene.Create_Node (World.Identifier & " moons");
         Moons      : constant Array_Of_Worlds := World.Moons;
         Ships      : Concorde.Ships.Lists.List;

      begin
         World_Node.Set_Entity (Rec.Entity);
         World_Node.Scale
              (Xi.Xi_Float (World.Radius));

         for Moon of Moons loop
            declare
               use Xi;
               Node : constant Xi.Node.Xi_Node :=
                        Moons_Node.Create_Child
                          (Moon.Name);
               Pos  : constant Newton.Vector_3 :=
                        Moon.Primary_Relative_Position;
            begin
               Node.Set_Position
                 (Xi_Float (Pos (1)),
                  Xi_Float (Pos (2)),
                  Xi_Float (Pos (3)));
               Node.Set_Entity (World_Entity (Moon));
            end;
         end loop;

         World.Get_Ships (Ships);
         Model.Ships.Clear;

         for Ship of Ships loop

            declare
               use Concorde.Ships.Xi_Model;
               Rec : constant Active_Ship :=
                       Activate_Ship
                         (Ship, Scene, Ships_Node);
               Camera : constant Xi.Camera.Xi_Camera :=
                          Local_Camera (Rec);
            begin
               Camera.Set_Position
                 (0.0, 0.0, 100.0);
               Camera.Look_At
                 (0.0, 1.0, 0.0, 0.0, 0.0, 0.0);
               Camera.Set_Viewport
                 (Model.Renderer.Full_Viewport);
               Camera.Perspective
                 (45.0, 10.0, 1.0e9);

               Model.Ships.Append (Rec);
            end;

            declare
               use Xi;
               Pos  : constant Newton.Vector_3 :=
                        Ship.Primary_Relative_Position;
               Selector : constant Concorde.Xi_UI.Select_Handler :=
                            new World_Ship_Selector'
                              (Model => Model,
                               Ship  => Ship);
            begin
               Concorde.Xi_UI .Selector_With_Text
                 (World_Node, Ship.Name,
                  Xi_Float (Pos (1)),
                  Xi_Float (Pos (2)),
                  Xi_Float (Pos (3)),
                  Selector);
            end;
--
--              Node.Set_Position
--                (Xi_Float (Pos (1) / 2.0),
--                 Xi_Float (Pos (2) / 2.0),
--                 Xi_Float (Pos (3)) / 2.0);
--              Node.Set_Entity
--                (Concorde.Xi_UI.Selector_With_Text
--                   (Ship.Name));
--              Node.Set_Billboard (True);
--              Node.Fixed_Pixel_Size (32.0, 32.0);
--              Text_Node.Set_Position
--                (Xi_Float (Pos (1) / 2.0 + 32.0),
--                 Xi_Float (Pos (2) / 2.0),
--                 Xi_Float (Pos (3)) / 2.0);
--              Text_Node.Set_Entity
--                (Concorde.Xi_UI.Selector_Text (Ship.Name));
--              Text_Node.Set_Billboard (True);
--              Text_Node.Fixed_Pixel_Size (32.0, 128.0);
         end loop;
      end;

      return Scene;
   end World_Scene;

end Concorde.Worlds.Xi_Model;
