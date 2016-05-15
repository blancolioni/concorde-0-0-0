with Ada.Calendar;
with Ada.Characters.Handling;

with Concorde.Hash_Table;
with Concorde.Watchers;

with Concorde.Solar_System;

with Lui.Colours;
with Lui.Models.Model_3D;
with Lui.Tables;

package body Concorde.Worlds.Tile_Models is

   type Map_Mode_Type is (Height_Mode, Temperature_Mode);

   Current_Map_Mode : Map_Mode_Type := Height_Mode;

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

   Sector_Column_Count : constant := 2;
   Sector_Row_Count    : constant := 8;

   subtype Sector_Column is Integer range 1 .. Sector_Column_Count;
   subtype Sector_Row is Integer range 1 .. Sector_Row_Count;

   type Sector_Table is
     new Lui.Tables.Root_Model_Table with
      record
         Selected : Boolean := False;
         World    : World_Type;
         Tile     : Concorde.Surfaces.Surface_Tile_Index;
      end record;

   overriding function Heading_Column_Text
     (Table : Sector_Table;
      Col   : Positive)
      return String
   is ((case Sector_Column (Col) is
           when 1 => "Property",
           when 2 => "Value"));

   overriding function Cell_Text
     (Table : Sector_Table;
      Row   : Positive;
      Col   : Positive)
      return String;

   overriding function Row_Count
     (Table : Sector_Table)
      return Natural
   is ((if Table.Selected then Sector_Row_Count else 0));

   type Sector_Table_Access is access all Sector_Table'Class;

   type Root_World_Model is
     new Lui.Models.Model_3D.Root_3D_Model
     and Concorde.Watchers.Watcher_Interface with
      record
         World             : World_Type;
         Needs_Render      : Boolean := True;
         Selected_Sector   : Concorde.Surfaces.Surface_Tile_Count := 0;
         Sector_Properties : Sector_Table_Access;
         Rotation          : Real    := 0.0;
         Rotate_Speed      : Real    := 0.0;   --  degrees per second
         Last_Update       : Ada.Calendar.Time;
      end record;

   overriding function Handle_Update
     (Model : in out Root_World_Model)
      return Boolean;

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
                             "kPa");
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
         declare
            use type Concorde.Features.Feature_Type;
            Ice_Sectors : Natural := 0;
         begin
            for Sector of World.Sectors.all loop
               if Sector.Feature = Concorde.Features.Ice then
                  Ice_Sectors := Ice_Sectors + 1;
               end if;
            end loop;
            Model.Add_Property ("Ice sectors",
                                Real (Ice_Sectors)
                                / Real (World.Sector_Count)
                                * 100.0,
                                "%");
         end;

         Model.Add_Property ("Cloud coverage",
                             World.Cloud_Cover * 100.0,
                             "%");

         for Atm of World.Atmosphere loop
            Model.Add_Property (Atm.Gas.Formula,
                                Atm.Fraction * 100.0,
                                "%");
         end loop;

      end if;
   end Add_Properties;

   ---------------
   -- Cell_Text --
   ---------------

   overriding function Cell_Text
     (Table : Sector_Table;
      Row   : Positive;
      Col   : Positive)
      return String
   is
   begin
      case Sector_Column (Col) is
         when 1 =>
            case Sector_Row (Row) is
               when 1 =>
                  return "Terrain";
               when 2 =>
                  return "Elevation";
               when 3 =>
                  return "Max Temperature";
               when 4 =>
                  return "Ave Temperature";
               when 5 =>
                  return "Min Temperature";
               when 6 =>
                  return "Resource";
               when 7 =>
                  return "Concentration";
               when 8 =>
                  return "Accessibility";
            end case;
         when 2 =>
            declare
               use type Concorde.Features.Feature_Type;
               use type Concorde.Terrain.Terrain_Type;
               use type Concorde.Commodities.Commodity_Type;
               Sector : Sector_Record renames
                          Table.World.Sectors (Table.Tile);
            begin
               case Sector_Row (Row) is
                  when 1 =>
                     if Sector.Feature /= null then
                        if Sector.Terrain /= null then
                           return Sector.Feature.Name
                             & " "
                             & Sector.Terrain.Name;
                        else
                           return Sector.Feature.Name;
                        end if;
                     elsif Sector.Terrain /= null then
                        return Sector.Terrain.Name;
                     else
                        return "-";
                     end if;
                  when 2 =>
                     return Integer'Image
                       (Integer (Sector.Height) * 100) & "m";
                  when 3 .. 5 =>
                     declare
                        Temp_Rec : Temperature_Record renames
                                     Sector.Temperature;
                        Temp_K   : constant Real :=
                                     (if Row = 3
                                      then Temp_Rec.High
                                      elsif Row = 4
                                      then Temp_Rec.Average
                                      else Temp_Rec.Low);
                        Temp_C   : constant Real := Temp_K - 273.15;
                     begin
                        return Lui.Approximate_Image
                          (Integer (Temp_C));
                     end;
                  when 6 =>
                     if Sector.Deposit.Resource /= null then
                        return Sector.Deposit.Resource.Name;
                     else
                        return "none";
                     end if;
                  when 7 =>
                     if Sector.Deposit.Resource /= null then
                        return Lui.Approximate_Image
                          (Sector.Deposit.Concentration * 100.0)
                          & "%";
                     else
                        return "-";
                     end if;
                  when 8 =>
                     if Sector.Deposit.Resource /= null then
                        return Lui.Approximate_Image
                          (Sector.Deposit.Accessibility * 100.0)
                          & "%";
                     else
                        return "-";
                     end if;
               end case;
            end;
      end case;
   end Cell_Text;

   ------------------
   -- Create_Scene --
   ------------------

   overriding procedure Create_Scene
     (Model  : in out Root_World_Model)
   is
      use Concorde.Surfaces;
      use Lui.Models.Model_3D.Matrices;

      Surface : constant Concorde.Surfaces.Surface_Type :=
                  Model.World.Surface;

      procedure Draw_Tile
        (Index  : Surface_Tile_Index;
         Colour : Lui.Colours.Colour_Type);

      ---------------
      -- Draw_Tile --
      ---------------

      procedure Draw_Tile
        (Index  : Surface_Tile_Index;
         Colour : Lui.Colours.Colour_Type)
      is
         Boundary : constant Tile_Vertex_Array :=
                      Surface.Tile_Boundary (Index);
         Height   : constant Height_Range :=
                      Model.World.Sectors (Index).Height;
         Factor   : constant Non_Negative_Real :=
                      (if True or else Height < 0
                       then 1.0
                       else 1.0 + Real (Height) / 1000.0);
      begin
         Model.Begin_Object (Positive (Index));
         Model.Begin_Surface (Colour);
         for V of Boundary loop
            Model.Vertex (V * Factor);
         end loop;
         Model.End_Surface;
         Model.End_Object;
      end Draw_Tile;

   begin

      Model.Rotate_Y (Model.Rotation);

      if Model.Selected_Sector /= 0 then
         Draw_Tile (Model.Selected_Sector, Selected_Colour);
      end if;

      for I in 1 .. Surface.Tile_Count loop
         declare
            use type Concorde.Features.Feature_Type;
            use type Concorde.Terrain.Terrain_Type;
            Raw_Height  : constant Height_Range :=
                            Model.World.Sectors (I).Height;
            Height      : constant Height_Range :=
                            (if Raw_Height < 0
                             then ((Raw_Height + 2) / 3) * 3 - 2
                             else Raw_Height);
            Ave_Temp    : constant Non_Negative_Real :=
                            Model.World.Sectors (I).Temperature.Average;
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
                              Model.World.Sectors (I).Feature;
            Terrain       : constant Concorde.Terrain.Terrain_Type :=
                              Model.World.Sectors (I).Terrain;

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
            Draw_Tile (I, Colour);
         end;
      end loop;

   end Create_Scene;

   -------------------
   -- Handle_Update --
   -------------------

   overriding function Handle_Update
     (Model : in out Root_World_Model)
      return Boolean
   is
      use Ada.Calendar;
      Now : constant Time := Clock;
      Dt  : constant Real := Real (Now - Model.Last_Update);
   begin
      Model.Rotation := Dt * Model.Rotate_Speed;
      Model.Last_Update := Now;
      return True;
   end Handle_Update;

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
      if Object_Id = 0 then
         null;
      elsif Object_Id = Natural (Model.Selected_Sector) then
         Model.Selected_Sector := 0;
         Model.Sector_Properties.Selected := False;
      else
         Model.Selected_Sector :=
           Concorde.Surfaces.Surface_Tile_Index (Object_Id);
         Model.Sector_Properties.Tile := Model.Selected_Sector;
         Model.Sector_Properties.Selected := True;
         Model.Needs_Render := True;
      end if;
      if False then
         if Current_Map_Mode = Temperature_Mode then
            Current_Map_Mode := Height_Mode;
         else
            Current_Map_Mode := Temperature_Mode;
         end if;
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
            Result.Sector_Properties :=
              new Sector_Table;
            Result.Sector_Properties.Initialise
              ("Selected Sector",
               Num_Rows => 0,
               Num_Cols => Sector_Column_Count);
            Result.Sector_Properties.World := World;
            Result.Sector_Properties.Selected := False;
            Result.Initialise
              (World.Name,
               Tables =>
                 (1 => Lui.Tables.Model_Table (Result.Sector_Properties)));
            Result.World := World;
            Result.Selected_Sector := 0;
            Result.Last_Update := Ada.Calendar.Clock;
            Result.Drag_Rotation_Behaviour
              (Y_Axis    => True,
               X_Axis    => False,
               Z_Axis    => False,
               Reverse_X => True,
               Reverse_Y => False,
               Reverse_Z => False);
            Add_Properties (Result);
            World_Models.Insert (World.Identifier, Result);
         end;
      end if;

      return Lui.Models.Object_Model (World_Models.Element (World.Identifier));
   end World_Tile_Model;

end Concorde.Worlds.Tile_Models;
