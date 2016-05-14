with Ada.Characters.Handling;

with Concorde.Hash_Table;
with Concorde.Watchers;

with Concorde.Solar_System;

with Lui.Colours;
with Lui.Models.Model_3D;

package body Concorde.Worlds.Tile_Models is

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

   type Root_World_Model is
     new Lui.Models.Model_3D.Root_3D_Model
     and Concorde.Watchers.Watcher_Interface with
      record
         World           : World_Type;
         Needs_Render    : Boolean := True;
         Selected_Sector : Natural := 0;
      end record;

   overriding procedure On_Object_Changed
     (Model  : in out Root_World_Model;
      Object : Concorde.Watchers.Watched_Object_Interface'Class);

   overriding procedure Create_Scene
     (Model  : in out Root_World_Model);

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
         declare
            Water_Sectors : Natural := 0;
         begin
            for Sector of World.Sectors.all loop
               if Sector.Height < 0 then
                  Water_Sectors := Water_Sectors + 1;
               end if;
            end loop;
            Model.Add_Property ("Water sectors",
                                Real (Water_Sectors)
                                / Real (World.Sector_Count)
                                * 100.0,
                                "%");
         end;

         Model.Add_Property ("Ice coverage",
                             World.Ice_Cover * 100.0,
                             "%");
         Model.Add_Property ("Cloud coverage",
                             World.Cloud_Cover * 100.0,
                             "%");
      end if;
   end Add_Properties;

   ------------------
   -- Create_Scene --
   ------------------

   overriding procedure Create_Scene
     (Model  : in out Root_World_Model)
   is
      Surface : constant Concorde.Surfaces.Surface_Type :=
                  Model.World.Surface;
   begin
      for I in 1 .. Surface.Tile_Count loop
--           Ada.Text_IO.Put_Line ("--- tile" & I'Img);
         Model.Begin_Object (Positive (I));

         declare
            use Concorde.Surfaces;
            Height : constant Height_Range :=
                       Model.World.Sectors
                         (Positive (I)).Height;
            Colour : constant Lui.Colours.Colour_Type :=
                       Lui.Colours.To_Colour
                         (Height_Red (Height),
                          Height_Green (Height),
                          Height_Blue (Height));
            Boundary : constant Tile_Vertex_Array :=
                         Surface.Tile_Boundary (I);
         begin
            Model.Begin_Surface (Colour);
            for V of Boundary loop
               Model.Vertex (V);
            end loop;
         end;
         Model.End_Surface;
         Model.End_Object;
      end loop;
   end Create_Scene;

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

   ---------------
   -- Select_XY --
   ---------------

   overriding procedure Select_XY
     (Model : in out Root_World_Model;
      X, Y  : Natural)
   is
      Object_Id : constant Natural := Model.Get_Object_Id (X, Y);
   begin
      if Object_Id = 0 or else Object_Id = Model.Selected_Sector then
         Model.Selected_Sector := 0;
      else
         Model.Selected_Sector := Object_Id;
      end if;
   end Select_XY;

   ----------------------
   -- World_Tile_Model --
   ----------------------

   function World_Tile_Model
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
   end World_Tile_Model;

end Concorde.Worlds.Tile_Models;
