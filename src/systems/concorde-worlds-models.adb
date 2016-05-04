with Lui.Colours;
with Lui.Rendering;

with Concorde.Hash_Table;
with Concorde.Watchers;

package body Concorde.Worlds.Models is

   Rendered_Sector_Width  : constant := 16;
   Rendered_Sector_Height : constant := 16;

   type Root_World_Model is
     new Lui.Models.Root_Object_Model
     and Concorde.Watchers.Watcher_Interface with
      record
         World          : World_Type;
         Needs_Render   : Boolean := True;
      end record;

   overriding procedure On_Object_Changed
     (Model  : in out Root_World_Model;
      Object : Concorde.Watchers.Watched_Object_Interface'Class);

   overriding function Handle_Update
     (Model    : in out Root_World_Model)
      return Boolean
   is (Model.Needs_Render);

   overriding procedure Render
     (Model    : in out Root_World_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class);

   overriding function Get_Drag_Behaviour
     (Model : Root_World_Model)
      return Lui.Models.Drag_Behaviour
   is (Lui.Models.Translation);

   type World_Model_Access is
     access all Root_World_Model'Class;

   package Model_Table is
     new Concorde.Hash_Table (World_Model_Access);

   World_Models : Model_Table.Map;

   -----------------------
   -- On_Object_Changed --
   -----------------------

   overriding procedure On_Object_Changed
     (Model  : in out Root_World_Model;
      Object : Concorde.Watchers.Watched_Object_Interface'Class)
   is
      pragma Unreferenced (Object);
   begin
      Model.Needs_Render := True;
   end On_Object_Changed;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Model    : in out Root_World_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class)
   is
      Sector_Index  : Positive := 1;
      Render_Height : constant Positive :=
                        Model.World.Half_Circle_Sectors
                          * Rendered_Sector_Height;
   begin
      for Latitude_Index in Model.World.Row_Length'Range loop
         declare
            Row_Top : constant Integer :=
                        Model.Height / 2 - Render_Height / 2
                          + (Latitude_Index - 1) * Rendered_Sector_Height;
            Length    : constant Positive :=
                          Model.World.Row_Length (Latitude_Index);
            Row_Width : constant Positive :=
                          Length * Rendered_Sector_Width;
         begin
            for Longitude_Index in 1 .. Length loop
               declare
                  use type Concorde.Features.Feature_Type;
                  Sector_Left : constant Integer :=
                                  Model.Width / 2
                                    - Row_Width / 2
                                  + (Longitude_Index - 1)
                                  * Rendered_Sector_Width;
                  Sector      : Sector_Record renames
                                  Model.World.Sectors (Sector_Index);
                  Colour      : constant Lui.Colours.Colour_Type :=
                                  (if Sector.Feature /= null
                                   then Sector.Feature.Colour
                                   else Sector.Terrain.Colour);
               begin
                  Renderer.Draw_Rectangle
                    (X      => Sector_Left,
                     Y      => Row_Top,
                     W      => Rendered_Sector_Width,
                     H      => Rendered_Sector_Height,
                     Colour => Colour,
                     Filled => True);
                  Renderer.Draw_Rectangle
                    (X      => Sector_Left,
                     Y      => Row_Top,
                     W      => Rendered_Sector_Width,
                     H      => Rendered_Sector_Height,
                     Colour => Lui.Colours.Black,
                     Filled => False);
                  Sector_Index := Sector_Index + 1;
               end;
            end loop;
         end;
      end loop;
   end Render;

   -----------------
   -- World_Model --
   -----------------

   function World_Model
     (World : World_Type)
      return Lui.Models.Object_Model
   is
   begin
      if not World_Models.Contains (World.Identifier) then
         declare
            Result : constant World_Model_Access := new Root_World_Model;
         begin
            Result.Initialise (World.Name);
            Result.World := World;
            World_Models.Insert (World.Identifier, Result);
         end;
      end if;

      return Lui.Models.Object_Model (World_Models.Element (World.Identifier));
   end World_Model;

end Concorde.Worlds.Models;
