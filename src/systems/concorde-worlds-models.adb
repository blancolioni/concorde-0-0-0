with Ada.Characters.Handling;

with Lui.Colours;
with Lui.Rendering;

with Concorde.Hash_Table;
with Concorde.Watchers;

with Concorde.Solar_System;

package body Concorde.Worlds.Models is

   Base_Sector_Width  : constant := 32.0;
   Base_Sector_Height : constant := 32.0;

   type Rendered_Sector is
      record
         X, Y            : Integer;
         Width, Height   : Natural;
         Sector_Index    : Positive;
      end record;

   package Rendered_Sector_Vectors is
     new Ada.Containers.Vectors (Positive, Rendered_Sector);

   type Root_World_Model is
     new Lui.Models.Root_Object_Model
     and Concorde.Watchers.Watcher_Interface with
      record
         World           : World_Type;
         Sectors         : Rendered_Sector_Vectors.Vector;
         Needs_Render    : Boolean := True;
         Selected_Sector : Natural := 0;
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

   overriding procedure Select_XY
     (Model : in out Root_World_Model;
      X, Y  : Natural);

   type World_Model_Access is
     access all Root_World_Model'Class;

   procedure Add_Properties
     (Model : World_Model_Access);

   function Category_Name
     (Category : World_Category)
      return String
   is (Ada.Characters.Handling.To_Lower
       (World_Category'Image (Category)));

   package Model_Table is
     new Concorde.Hash_Table (World_Model_Access);

   World_Models : Model_Table.Map;

   --------------------
   -- Add_Properties --
   --------------------

   procedure Add_Properties
     (Model : World_Model_Access)
   is
      use Concorde.Solar_System;
      World : constant World_Type := Model.World;
   begin
      Model.Add_Property ("Name", World.Name);
      Model.Add_Property ("Category",
                          Category_Name (World.Category));
--        Model.Add_Property ("Habitability",
--                            Conflict.Planets.Habitability (P) * 100.0,
--                            "%");
--        Model.Add_Property ("Population",
--                            Conflict.Planets.Get_Population (P));
      Model.Add_Property ("Orbit", World.Semimajor_Axis / Earth_Orbit,
                          "AU");
      Model.Add_Property ("Year",
                          World.Period / 3600.0 / 24.0 / Earth_Sidereal_Year,
                          "earth years");
      Model.Add_Property ("Day", World.Day_Length / 3600.0,
                          "hours");
      Model.Add_Property ("Radius", World.Radius / Earth_Radius,
                          "earths");
      Model.Add_Property ("Mass", World.Mass / Earth_Mass,
                          "earths");
      Model.Add_Property ("Surface g", World.Surface_Gravity,
                          "earth");

      if not World.Gas_Giant then
         Model.Add_Property ("Surface pressure", World.Surface_Pressure,
                             "millibar");
         Model.Add_Property ("Min temperature",
                             World.Min_Temperature - 273.15,
                             "℃");
         Model.Add_Property ("Max temperature",
                             World.Max_Temperature - 273.15,
                             "℃");
         Model.Add_Property ("Nighttime low",
                             World.Nighttime_Low - 273.15,
                             "℃");
         Model.Add_Property ("Daytime high",
                             World.Daytime_High - 273.15,
                             "℃");
         Model.Add_Property ("Greenhouse contribution",
                             World.Greenhouse_Rise,
                             "℃");
         Model.Add_Property ("Water coverage",
                             World.Hydrosphere * 100.0,
                             "%");
         Model.Add_Property ("Ice coverage",
                             World.Ice_Cover * 100.0,
                             "%");
         Model.Add_Property ("Cloud coverage",
                             World.Cloud_Cover * 100.0,
                             "%");
      end if;
   end Add_Properties;

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
      Sector_Size_Factor : constant Non_Negative_Real :=
                             1.0 / Model.Eye_Z;
      Sector_Width       : constant Positive :=
                             Natural'Max
                               (Natural
                                  (Sector_Size_Factor
                                   * Base_Sector_Width),
                                1);
      Sector_Height       : constant Positive :=
                              Natural'Max
                                (Natural
                                   (Sector_Size_Factor
                                    * Base_Sector_Height),
                                 1);
      Render_Height       : constant Positive :=
                              Model.World.Half_Circle_Sectors
                                * Sector_Height;
   begin
      Model.Sectors.Clear;
      for Latitude_Index in Model.World.Row_Length'Range loop
         declare
            Row_Top : constant Integer :=
                        Model.Height / 2 - Render_Height / 2
                          + (Latitude_Index - 1) * Sector_Height;
            Length    : constant Positive :=
                          Model.World.Row_Length (Latitude_Index);
            Row_Width : constant Positive :=
                          Length * Sector_Width;
         begin
            for Longitude_Index in 1 .. Length loop
               declare
                  use type Concorde.Features.Feature_Type;
                  Sector_Left : constant Integer :=
                                  Model.Width / 2
                                    - Row_Width / 2
                                  + (Longitude_Index - 1)
                                  * Sector_Width;
                  Sector      : Sector_Record renames
                                  Model.World.Sectors (Sector_Index);
                  Colour      : constant Lui.Colours.Colour_Type :=
                                  (if Sector.Feature /= null
                                   then Sector.Feature.Colour
                                   else Sector.Terrain.Colour);
                  Border_Colour : constant Lui.Colours.Colour_Type :=
                                    Lui.Colours.Black;
               begin
                  Renderer.Draw_Rectangle
                    (X      => Sector_Left,
                     Y      => Row_Top,
                     W      => Sector_Width,
                     H      => Sector_Height,
                     Colour => Colour,
                     Filled => True);
                  if Sector_Width > 8 and then Sector_Height > 8 then
                     Renderer.Draw_Rectangle
                       (X      => Sector_Left,
                        Y      => Row_Top,
                        W      => Sector_Width,
                        H      => Sector_Height,
                        Colour => Border_Colour,
                        Filled => False);
                  end if;
                  Model.Sectors.Append
                    ((Sector_Left, Row_Top, Sector_Width, Sector_Height,
                     Sector_Index));
                  Sector_Index := Sector_Index + 1;
               end;
            end loop;
         end;
      end loop;

      if Model.Selected_Sector /= 0 then
         declare
            Highlight : Rendered_Sector renames
                          Model.Sectors (Model.Selected_Sector);
         begin
            Renderer.Draw_Rectangle
              (X      => Highlight.X,
               Y      => Highlight.Y,
               W      => Highlight.Width,
               H      => Highlight.Height,
               Colour => (0.6, 0.6, 0.0, 1.0),
               Filled => False);

            declare
               procedure Draw_Connection
                 (To : Positive;
                  Cost : Non_Negative_Real);

               ---------------------
               -- Draw_Connection --
               ---------------------

               procedure Draw_Connection
                 (To   : Positive;
                  Cost : Non_Negative_Real)
               is
                  pragma Unreferenced (Cost);
                  Target : Rendered_Sector renames
                             Model.Sectors (To);
                  X1     : constant Integer :=
                             Highlight.X + Highlight.Width / 2;
                  Y1     : constant Integer :=
                             Highlight.Y + Highlight.Height / 2;
                  X2     : constant Integer :=
                             Target.X + Target.Width / 2;
                  Y2     : constant Integer :=
                             Target.Y + Target.Height / 2;
               begin
                  Renderer.Draw_Line
                    (X1, Y1, X2, Y2, Lui.Colours.White, 1);
               end Draw_Connection;

            begin
               Model.World.Graph.Iterate_Edges
                 (Model.Selected_Sector, Draw_Connection'Access);
            end;
         end;
      end if;

   end Render;

   ---------------
   -- Select_XY --
   ---------------

   overriding procedure Select_XY
     (Model : in out Root_World_Model;
      X, Y  : Natural)
   is
   begin
      for Sector of Model.Sectors loop
         if X in Sector.X .. Sector.X + Sector.Width
           and then Y in Sector.Y .. Sector.Y + Sector.Height
         then
            Model.Selected_Sector := Sector.Sector_Index;
            Model.Needs_Render := True;
            return;
         end if;
      end loop;
   end Select_XY;

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
            Result.Selected_Sector := World.Sectors'Length / 2;
            Add_Properties (Result);
            World_Models.Insert (World.Identifier, Result);
         end;
      end if;

      return Lui.Models.Object_Model (World_Models.Element (World.Identifier));
   end World_Model;

end Concorde.Worlds.Models;
