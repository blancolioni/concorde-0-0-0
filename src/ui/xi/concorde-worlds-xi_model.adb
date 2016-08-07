with Xi.Entity.Manual;
with Xi.Matrices;
with Xi.Render_Operation;
with Xi.Shader;
with Xi.Shapes;

with Lui.Colours;

with Concorde.Hash_Table;

with Concorde.Xi_UI.Assets;
with Concorde.Xi_UI.Colours;
with Concorde.Xi_UI.Shaders;

--  with Concorde.Solar_System;
--  with Concorde.Money;
--  with Concorde.Quantities;

with Concorde.Empires;

package body Concorde.Worlds.Xi_Model is

   type Map_Mode_Type is (Height_Mode, Temperature_Mode);

   Current_Map_Mode : constant Map_Mode_Type := Height_Mode;

   type Colour_Element_Array is
     array (Height_Range) of Lui.Colours.Colour_Byte;

   Height_Red     : constant Colour_Element_Array :=
                      (0,
                       0, 0, 0, 0, 0, 0, 0, 34,
                       68, 102, 119, 136, 153, 170, 187, 0,
                       34, 34, 119, 187, 255, 238, 221, 204,
                       187, 170, 153, 136, 119, 85, 68, 255,
                       250, 245, 240, 235, 230, 225, 220, 215,
                       210, 205, 200, 195, 190, 185, 180, 175);

   Height_Green   : constant Colour_Element_Array :=
                      (0,
                       0, 17, 51, 85, 119, 153, 204, 221,
                       238, 255, 255, 255, 255, 255, 255, 68,
                       102, 136, 170, 221, 187, 170, 136, 136,
                       102, 85, 85, 68, 51, 51, 34, 255,
                       250, 245, 240, 235, 230, 225, 220, 215,
                       210, 205, 200, 195, 190, 185, 180, 175);

   Height_Blue    : constant Colour_Element_Array :=
                      (0,
                       68, 102, 136, 170, 187, 221, 255, 255,
                       255, 255, 255, 255, 255, 255, 255, 0,
                       0, 0, 0, 0, 34, 34, 34, 34,
                       34, 34, 34, 34, 34, 17, 0, 255,
                       250, 245, 240, 235, 230, 225, 220, 215,
                       210, 205, 200, 195, 190, 185, 180, 175);

   type Temperature_Palette_Array is
     array (250 .. 339, 1 .. 3) of Lui.Colours.Colour_Byte;

   Temperature_Palette : constant Temperature_Palette_Array :=
                           ((255, 14, 240),
                            (255, 13, 240),
                            (255, 12, 240),
                            (255, 11, 240),
                            (255, 10, 240),
                            (255, 9, 240),
                            (255, 8, 240),
                            (255, 7, 240),
                            (255, 6, 240),
                            (255, 5, 240),
                            (255, 4, 240),
                            (255, 3, 240),
                            (255, 2, 240),
                            (255, 1, 240),
                            (255, 0, 240),
                            (255, 0, 224),
                            (255, 0, 208),
                            (255, 0, 192),
                            (255, 0, 176),
                            (255, 0, 160),
                            (255, 0, 144),
                            (255, 0, 128),
                            (255, 0, 112),
                            (255, 0, 96),
                            (255, 0, 80),
                            (255, 0, 64),
                            (255, 0, 48),
                            (255, 0, 32),
                            (255, 0, 16),
                            (255, 0, 0),
                            (255, 10, 0),
                            (255, 20, 0),
                            (255, 30, 0),
                            (255, 40, 0),
                            (255, 50, 0),
                            (255, 60, 0),
                            (255, 70, 0),
                            (255, 80, 0),
                            (255, 90, 0),
                            (255, 100, 0),
                            (255, 110, 0),
                            (255, 120, 0),
                            (255, 130, 0),
                            (255, 140, 0),
                            (255, 150, 0),
                            (255, 160, 0),
                            (255, 170, 0),
                            (255, 180, 0),
                            (255, 190, 0),
                            (255, 200, 0),
                            (255, 210, 0),
                            (255, 220, 0),
                            (255, 230, 0),
                            (255, 240, 0),
                            (255, 250, 0),
                            (253, 255, 0),
                            (215, 255, 0),
                            (176, 255, 0),
                            (138, 255, 0),
                            (101, 255, 0),
                            (62, 255, 0),
                            (23, 255, 0),
                            (0, 255, 16),
                            (0, 255, 54),
                            (0, 255, 92),
                            (0, 255, 131),
                            (0, 255, 168),
                            (0, 255, 208),
                            (0, 255, 244),
                            (0, 228, 255),
                            (0, 212, 255),
                            (0, 196, 255),
                            (0, 180, 255),
                            (0, 164, 255),
                            (0, 148, 255),
                            (0, 132, 255),
                            (0, 116, 255),
                            (0, 100, 255),
                            (0, 84, 255),
                            (0, 68, 255),
                            (0, 50, 255),
                            (0, 34, 255),
                            (0, 18, 255),
                            (0, 2, 255),
                            (0, 0, 255),
                            (1, 0, 255),
                            (2, 0, 255),
                            (3, 0, 255),
                            (4, 0, 255),
                            (5, 0, 255));

   Selected_Colour : constant Lui.Colours.Colour_Type :=
                       (0.8, 0.7, 0.1, 0.5);
   pragma Unreferenced (Selected_Colour);

   type Rendered_World_Record is
      record
         World   : World_Type;
         Entity  : Xi.Entity.Xi_Entity;
      end record;

   package Rendered_World_Table is
     new Concorde.Hash_Table (Rendered_World_Record);

   Rendered_Worlds : Rendered_World_Table.Map;
   pragma Unreferenced (Rendered_Worlds);

   procedure Create_Tiles
     (World       : World_Type;
      Parent_Node : Xi.Node.Xi_Node);

   ------------------
   -- Create_Tiles --
   ------------------

   procedure Create_Tiles
     (World       : World_Type;
      Parent_Node : Xi.Node.Xi_Node)
   is
      Surface : constant Concorde.Surfaces.Surface_Type :=
                  World.Surface;

      Shader : constant Xi.Shader.Xi_Shader :=
                 Concorde.Xi_UI.Shaders.Shader ("world");

      procedure Draw_Tile
        (Index  : Concorde.Surfaces.Surface_Tile_Index;
         Colour : Lui.Colours.Colour_Type);

      ---------------
      -- Draw_Tile --
      ---------------

      procedure Draw_Tile
        (Index  : Concorde.Surfaces.Surface_Tile_Index;
         Colour : Lui.Colours.Colour_Type)
      is
         use Xi;
         Boundary : constant Concorde.Surfaces.Tile_Vertex_Array :=
                      Surface.Tile_Boundary (Index);
         Entity   : Xi.Entity.Manual.Xi_Manual_Entity;
         Node     : constant Xi.Node.Xi_Node :=
                      Parent_Node.Create_Child ("Tile" & Index'Img);

         --           Height   : constant Height_Range :=
--                        Model.World.Sectors (Index).Height;
--           Factor   : constant Non_Negative_Real :=
--                        (if True or else Height < 0
--                         then 1.0
--                         else 1.0 + Real (Height) / 1000.0);

         function Vertex
           (V : Concorde.Surfaces.Vector_3)
            return Xi.Matrices.Vector_3
         is ((Xi_Float (V (1)),
              Xi_Float (V (2)),
              Xi_Float (V (3))));

      begin
         Xi.Entity.Manual.Xi_New (Entity);
         Entity.Bind_Shader
           (Vertices => Shader.Declare_Attribute_Value ("vPosition"),
            Colors   => Shader.Declare_Attribute_Value ("vColor"));

         Entity.Begin_Operation (Xi.Render_Operation.Triangle_Fan);
         Entity.Color (Concorde.Xi_UI.Colours.To_Xi_Color (Colour));

         for V of Boundary loop
            Entity.Vertex (Vertex (V));
         end loop;

         Entity.End_Operation;
         Node.Set_Entity (Entity);
      end Draw_Tile;

   begin

      Parent_Node.Set_Shader (Shader);

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
            Ave_Temp    : constant Non_Negative_Real :=
                            World.Sectors (I).Temperature.Average;
            Int_Temp    : constant Integer :=
                            Integer'Max
                              (Temperature_Palette'First (1),
                               Integer'Min
                                 (Temperature_Palette'Last (1),
                                  Integer (Ave_Temp)));
            Temp_Colour : constant Lui.Colours.Colour_Type :=
                            Lui.Colours.To_Colour
                              (Temperature_Palette (Int_Temp, 1),
                               Temperature_Palette (Int_Temp, 2),
                               Temperature_Palette (Int_Temp, 3));
            Height_Colour : constant Lui.Colours.Colour_Type :=
                              Lui.Colours.To_Colour
                                (Height_Red (Height),
                                 Height_Green (Height),
                                 Height_Blue (Height));
            Feature       : constant Concorde.Features.Feature_Type :=
                              World.Sectors (I).Feature;
            Terrain       : constant Concorde.Terrain.Terrain_Type :=
                              World.Sectors (I).Terrain;
            Owner : constant access constant
              Concorde.Empires.Root_Empire_Type'Class :=
                (if not World.Sectors (I).Installations.Is_Empty
                 then World.Owner
                 else null);
            Colour     : constant Lui.Colours.Colour_Type :=
                           (case Current_Map_Mode is
                               when Height_Mode      =>
                              (if Feature /= null
                               then Feature.Colour
                               elsif Terrain /= null
                               then Terrain.Colour
                               else Height_Colour),
                               when Temperature_Mode =>
                                  Temp_Colour);
         begin
            if Owner /= null then
               Draw_Tile (I, Lui.Colours.Apply_Alpha (Owner.Colour, 0.8));
            end if;
            Draw_Tile (I, Colour);
         end;
      end loop;

   end Create_Tiles;

   ------------------
   -- World_Entity --
   ------------------

   procedure Load_World
     (World       : World_Type;
      Parent_Node : Xi.Node.Xi_Node)
   is
   begin
      World.Check_Loaded;
      if World.Category in Jovian_World then
         Parent_Node.Set_Shader (Concorde.Xi_UI.Shaders.Shader ("star"));
         Parent_Node.Set_Entity
           (Xi.Shapes.Icosohedral_Sphere (1));
         Parent_Node.Entity.Set_Texture
           (Concorde.Xi_UI.Assets.Texture ("gas_giant_1x2048"));
         Parent_Node.Entity.Bind_Shader
           (Vertices =>
              Parent_Node.Shader.Declare_Attribute_Value ("vPosition"),
            Textures =>
              Parent_Node.Shader.Declare_Attribute_Value ("texture_coord"));
         Parent_Node.Entity.Texture.Set_Uniform
           (Parent_Node.Shader.Declare_Uniform_Value ("tex"));
      else
         Create_Tiles (World, Parent_Node);
      end if;
   end Load_World;

end Concorde.Worlds.Xi_Model;
