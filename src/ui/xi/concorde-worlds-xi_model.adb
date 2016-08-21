with Xi.Assets;
with Xi.Entity;
with Xi.Materials.Material;
with Xi.Materials.Pass;
with Xi.Matrices;
with Xi.Render_Operation;
with Xi.Scene;
with Xi.Shapes;
with Xi.Value;

with Lui.Colours;

with Concorde.Hash_Table;
with Concorde.Transitions;

with Concorde.Worlds.Tables;

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

   function Create_Tiles
     (World       : World_Type)
      return Xi.Entity.Xi_Entity;

   function World_Scene
     (World : World_Type;
      Model : Concorde.Xi_UI.Root_Xi_Model'Class)
      return Xi.Scene.Xi_Scene;

   function World_Entity
     (World : World_Type)
      return Xi.Entity.Xi_Entity;

   function Height_Material
     (Height : Height_Range)
      return Xi.Materials.Material.Xi_Material;

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

   procedure Transit_To_World
     (World     : Concorde.Worlds.World_Type;
      Model     : in out Concorde.Xi_UI.Root_Xi_Model'Class)
   is
      use Xi;
      use type Xi.Scene.Xi_Scene;
      use Concorde.Geometry;
      use type Concorde.Worlds.World_Type;
      Scene : constant Xi.Scene.Xi_Scene := World_Scene (World, Model);
      World_Transition : constant Concorde.Transitions.Transition_Type :=
                           new Concorde.Transitions.Root_Transition_Type;
      Target_Position : constant Xi.Matrices.Vector_3 :=
                          (0.0, Xi_Float (World.Radius),
                           Xi_Float (World.Radius) + 1_000_000.0);
   begin
      Scene.Active_Camera.Set_Viewport (Model.Window.Viewport);
      Scene.Active_Camera.Set_Position
        (0.0, 0.0, 224398100.0);
      Scene.Active_Camera.Perspective
        (60.0, 10.0, 1.0e9);
      Scene.Active_Camera.Look_At (0.0, 0.0, 0.0);

      World_Transition.Create
        (Scene              => Scene,
         Target_Position    => Target_Position,
         Target_Orientation => Scene.Active_Camera.Orientation,
         Acceleration       => 75.0e5,
         Max_Velocity       => 75.0e6);
      Model.Add_Transition (World_Transition);
   end Transit_To_World;

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
      else
         Entity := Create_Tiles (World);
      end if;
      return Entity;
   end World_Entity;

   -----------------
   -- World_Scene --
   -----------------

   function World_Scene
     (World : World_Type;
      Model : Concorde.Xi_UI.Root_Xi_Model'Class)
      return Xi.Scene.Xi_Scene
   is
      pragma Unreferenced (Model);
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
         Node : constant Xi.Node.Xi_Node :=
                  Scene.Create_Node (World.Identifier);
      begin
         Node.Set_Entity (Rec.Entity);
         Node.Scale
           (Xi.Xi_Float (World.Radius));
      end;

      return Scene;
   end World_Scene;

end Concorde.Worlds.Xi_Model;
