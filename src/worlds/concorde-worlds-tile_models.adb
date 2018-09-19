with Ada.Calendar;
with Ada.Characters.Handling;
with Ada.Containers.Vectors;

with Lui.Colors;
with Lui.Models.Model_3D;
with Lui.Tables;

with Concorde.Hash_Table;
with Concorde.Watchers;

with Concorde.Solar_System;
with Concorde.Money;
with Concorde.Quantities;

with Concorde.Factions;
with Concorde.Ships.Models;

with Concorde.Commodities.Db;

package body Concorde.Worlds.Tile_Models is

   type Map_Mode_Type is (Height_Mode, Temperature_Mode);

   Current_Map_Mode : Map_Mode_Type := Height_Mode;

   type Color_Element_Array is
     array (Height_Range) of Lui.Colors.Color_Byte;

   Height_Red     : constant Color_Element_Array :=
                      (0,
                       0, 0, 0, 0, 0, 0, 0, 34,
                       68, 102, 119, 136, 153, 170, 187, 0,
                       34, 34, 119, 187, 255, 238, 221, 204,
                       187, 170, 153, 136, 119, 85, 68, 255,
                       250, 245, 240, 235, 230, 225, 220, 215,
                       210, 205, 200, 195, 190, 185, 180, 175);

   Height_Green   : constant Color_Element_Array :=
                      (0,
                       0, 17, 51, 85, 119, 153, 204, 221,
                       238, 255, 255, 255, 255, 255, 255, 68,
                       102, 136, 170, 221, 187, 170, 136, 136,
                       102, 85, 85, 68, 51, 51, 34, 255,
                       250, 245, 240, 235, 230, 225, 220, 215,
                       210, 205, 200, 195, 190, 185, 180, 175);

   Height_Blue    : constant Color_Element_Array :=
                      (0,
                       68, 102, 136, 170, 187, 221, 255, 255,
                       255, 255, 255, 255, 255, 255, 255, 0,
                       0, 0, 0, 0, 34, 34, 34, 34,
                       34, 34, 34, 34, 34, 17, 0, 255,
                       250, 245, 240, 235, 230, 225, 220, 215,
                       210, 205, 200, 195, 190, 185, 180, 175);

   type Temperature_Palette_Array is
     array (250 .. 339, 1 .. 3) of Lui.Colors.Color_Byte;

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

   Selected_Color : constant Lui.Colors.Color_Type :=
                       (0.8, 0.7, 0.1, 0.5);

   Sector_Column_Count : constant := 2;
   Sector_Row_Count    : constant := 10;

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

   subtype Pop_Column is Integer range 1 .. 3;

   type Pop_Table is
     new Lui.Tables.Root_Model_Table with
      record
         World : World_Type;
         Pops  : Concorde.People.Pops.Lists.List;
      end record;

   type Pop_Table_Access is access all Pop_Table'Class;

   overriding function Heading_Column_Text
     (Table : Pop_Table;
      Col   : Positive)
      return String
   is ((case Pop_Column (Col) is
           when 1 => "Group",
           when 2 => "Size",
           when 3 => "Cash"));

   overriding function Cell_Text
     (Table : Pop_Table;
      Row   : Positive;
      Col   : Positive)
      return String;

   overriding function Row_Count
     (Table : Pop_Table)
      return Natural
   is (Natural (Table.Pops.Length));

   subtype Installation_Column is Integer range 1 .. 2;

   type Installation_Table is
     new Lui.Tables.Root_Model_Table with
      record
         World : World_Type;
         Installations : Concorde.Installations.Lists.List;
      end record;

   type Installation_Table_Access is
     access all Installation_Table'Class;

   overriding function Heading_Column_Text
     (Table : Installation_Table;
      Col   : Positive)
      return String
   is ((case Installation_Column (Col) is
           when 1 => "Facility",
           when 2 => "Cash"));

   overriding function Cell_Text
     (Table : Installation_Table;
      Row   : Positive;
      Col   : Positive)
      return String;

   overriding function Row_Count
     (Table : Installation_Table)
      return Natural
   is (Natural (Table.Installations.Length));

   subtype Market_Column is Integer range 1 .. 4;

   package Row_Map_Vectors is
     new Ada.Containers.Vectors
       (Positive,
        Concorde.Commodities.Commodity_Type,
        Concorde.Commodities."=");

   type Market_Table is
     new Lui.Tables.Root_Model_Table with
      record
         World   : World_Type;
         Row_Map : Row_Map_Vectors.Vector;
      end record;

   overriding function Heading_Column_Text
     (Table : Market_Table;
      Col   : Positive)
      return String
   is ((case Market_Column (Col) is
           when 1 => "Commodity",
           when 2 => "Price",
           when 3 => "Supply",
           when 4 => "Demand"));

   overriding function Cell_Text
     (Table : Market_Table;
      Row   : Positive;
      Col   : Positive)
      return String;

   subtype Ship_Column is Integer range 1 .. 4;

   type Ship_Table is
     new Lui.Tables.Root_Model_Table with
      record
         World : World_Type;
      end record;

   overriding function Heading_Column_Text
     (Table : Ship_Table;
      Col   : Positive)
      return String
   is ((case Ship_Column (Col) is
           when 1 => "Id",
           when 2 => "Name",
           when 3 => "Owner",
           when 4 => "Destination"));

   overriding function Cell_Text
     (Table : Ship_Table;
      Row   : Positive;
      Col   : Positive)
      return String;

   overriding function Row_Count
     (Table : Ship_Table)
      return Natural
   is (Natural (Table.World.Ships.Length));

   overriding function Row_Model
     (Table : Ship_Table;
      Row   : Positive)
      return access Lui.Models.Root_Object_Model'Class;

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
         Pops              : access Pop_Table;
         Installations     : access Installation_Table;
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
                  return "Latitude";
               when 2 =>
                  return "Longitude";
               when 3 =>
                  return "Terrain";
               when 4 =>
                  return "Elevation";
               when 5 =>
                  return "Max Temperature";
               when 6 =>
                  return "Ave Temperature";
               when 7 =>
                  return "Min Temperature";
               when 8 =>
                  return "Resource";
               when 9 =>
                  return "Concentration";
               when 10 =>
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
                     declare
                        Latitude : constant Real :=
                                     Table.World.Surface.Latitude (Table.Tile);
                        NS : constant String :=
                                     (if Latitude >= 0.0 then "N" else "S");
                     begin
                        return Lui.Approximate_Image (abs Latitude)
                          & " " & NS;
                     end;
                  when 2 =>
                     declare
                        Longitude : constant Real :=
                                      Table.World.Surface.Longitude
                                        (Table.Tile);
                        EW       : constant String :=
                                      (if Longitude >= 0.0
                                       then "E" else "W");
                     begin
                        return Lui.Approximate_Image (abs Longitude)
                          & " " & EW;
                     end;
                  when 3 =>
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
                  when 4 =>
                     return Integer'Image
                       (Integer (Sector.Height) * 100) & "m";
                  when 5 .. 7 =>
                     declare
                        Temp_Rec : Temperature_Record renames
                                     Sector.Temperature;
                        Temp_K   : constant Real :=
                                     (if Row = 5
                                      then Temp_Rec.High
                                      elsif Row = 6
                                      then Temp_Rec.Average
                                      else Temp_Rec.Low);
                        Temp_C   : constant Real := Temp_K - 273.15;
                     begin
                        return Lui.Approximate_Image
                          (Integer (Temp_C));
                     end;
                  when 8 =>
                     if Sector.Deposit.Resource /= null then
                        return Sector.Deposit.Resource.Name;
                     else
                        return "none";
                     end if;
                  when 9 =>
                     if Sector.Deposit.Resource /= null then
                        return Lui.Approximate_Image
                          (Sector.Deposit.Concentration * 100.0)
                          & "%";
                     else
                        return "-";
                     end if;
                  when 10 =>
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

   ---------------
   -- Cell_Text --
   ---------------

   overriding function Cell_Text
     (Table : Market_Table;
      Row   : Positive;
      Col   : Positive)
      return String
   is
      use Concorde.Commodities;
      Commodity : constant Commodity_Type := Table.Row_Map (Row);
   begin
      case Market_Column (Col) is
         when 1 =>
            return Commodity.Name;
         when 2 =>
            if Table.World.Has_Market then
               return Concorde.Money.Image
                 (Table.World.Market.Current_Price (Commodity));
            else
               return "-";
            end if;
         when 3 =>
            if Table.World.Has_Market then
               return Concorde.Quantities.Image
                 (Table.World.Market.Last_Supply (Commodity));
            else
               return "-";
            end if;
         when 4 =>
            if Table.World.Has_Market then
               return Concorde.Quantities.Image
                 (Table.World.Market.Last_Demand (Commodity));
            else
               return "-";
            end if;
      end case;
   end Cell_Text;

   ---------------
   -- Cell_Text --
   ---------------

   overriding function Cell_Text
     (Table : Pop_Table;
      Row   : Positive;
      Col   : Positive)
      return String
   is
      use Concorde.People.Pops.Lists;
      Position : Cursor := Table.Pops.First;
   begin
      for I in 2 .. Row loop
         Next (Position);
      end loop;

      declare
         Pop : constant Concorde.People.Pops.Pop_Type :=
                 Element (Position);
      begin
         case Pop_Column (Col) is
            when 1 =>
               return (if Pop.Rich then "Rich"
                       elsif Pop.Middle_Class then "Middle Class"
                       else "Poor");
            when 2 =>
               return Lui.Approximate_Image (Natural (Pop.Size));
            when 3 =>
               return Concorde.Money.Image (Pop.Cash);
         end case;
      end;
   end Cell_Text;

   ---------------
   -- Cell_Text --
   ---------------

   overriding function Cell_Text
     (Table : Installation_Table;
      Row   : Positive;
      Col   : Positive)
      return String
   is
      use Concorde.Installations.Lists;
      Position : Cursor := Table.Installations.First;
   begin
      for I in 2 .. Row loop
         Next (Position);
      end loop;

      declare
         Installation : constant Concorde.Installations.Installation_Type :=
                          Element (Position);
      begin
         case Installation_Column (Col) is
            when 1 =>
               return Installation.Facility.Name;
            when 2 =>
               return Concorde.Money.Image (Installation.Cash);
         end case;
      end;
   end Cell_Text;

   ---------------
   -- Cell_Text --
   ---------------

   overriding function Cell_Text
     (Table : Ship_Table;
      Row   : Positive;
      Col   : Positive)
      return String
   is
      use Concorde.Ships.Lists;
      Position : Cursor := Table.World.Ships.First;
   begin
      for I in 2 .. Row loop
         Next (Position);
      end loop;

      declare
         Ship : constant Concorde.Ships.Ship_Type :=
                  Element (Position);
      begin
         case Ship_Column (Col) is
            when 1 =>
               return Ship.Identifier;
            when 2 =>
               return Ship.Name;
            when 3 =>
               return Ship.Owner.Name;
            when 4 =>
               if Ship.Has_Destination then
                  return Ship.Destination.Name;
               else
                  return "-";
               end if;
         end case;
      end;
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
         Color : Lui.Colors.Color_Type);

      ---------------
      -- Draw_Tile --
      ---------------

      procedure Draw_Tile
        (Index  : Surface_Tile_Index;
         Color : Lui.Colors.Color_Type)
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
         Model.Begin_Surface (Color);
         for V of Boundary loop
            Model.Vertex (V * Factor);
         end loop;
         Model.End_Surface;
         Model.End_Object;
      end Draw_Tile;

   begin

      Model.Rotate_Y (Model.Rotation);

      if Model.Selected_Sector /= 0 then
         Draw_Tile (Model.Selected_Sector, Selected_Color);
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
            Temp_Color : constant Lui.Colors.Color_Type :=
                            Lui.Colors.To_Color
                              (Temperature_Palette (Int_Temp, 1),
                               Temperature_Palette (Int_Temp, 2),
                               Temperature_Palette (Int_Temp, 3));
            Height_Color : constant Lui.Colors.Color_Type :=
                              Lui.Colors.To_Color
                                (Height_Red (Height),
                                 Height_Green (Height),
                                 Height_Blue (Height));
            Feature       : constant Concorde.Features.Feature_Type :=
                              Model.World.Sectors (I).Feature;
            Terrain       : constant Concorde.Terrain.Terrain_Type :=
                              Model.World.Sectors (I).Terrain;
            Owner : constant access constant
              Concorde.Factions.Root_Faction_Type'Class :=
                (if not Model.World.Sectors (I).Installations.Is_Empty
                 then Model.World.Owner
                 else null);
            Color     : constant Lui.Colors.Color_Type :=
                           (case Current_Map_Mode is
                               when Height_Mode      =>
                              (if Feature /= null
                               then Feature.Color
                               elsif Terrain /= null
                               then Terrain.Color
                               else Height_Color),
                               when Temperature_Mode =>
                                  Temp_Color);
         begin
            if Owner /= null then
               Draw_Tile (I, Lui.Colors.Apply_Alpha (Owner.Color, 0.8));
            end if;
            Draw_Tile (I, Color);
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
   -- Row_Model --
   ---------------

   overriding function Row_Model
     (Table : Ship_Table;
      Row   : Positive)
      return access Lui.Models.Root_Object_Model'Class
   is
      use Concorde.Ships.Lists;
      Position : Cursor := Table.World.Ships.First;
   begin
      for I in 2 .. Row loop
         Next (Position);
      end loop;

      declare
         Ship : constant Concorde.Ships.Ship_Type :=
                  Element (Position);
      begin
         return Concorde.Ships.Models.Create_Ship_Model
           (Ship);
      end;

   end Row_Model;

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
      else
         Model.Selected_Sector :=
           Concorde.Surfaces.Surface_Tile_Index (Object_Id);
         Model.Sector_Properties.Tile := Model.Selected_Sector;
         Model.Pops.Pops :=
           Model.World.Sectors (Model.Selected_Sector).Pops;
         Model.Installations.Installations :=
           Model.World.Sectors (Model.Selected_Sector).Installations;
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

            M_Table : Market_Table;
            M       : Lui.Tables.Model_Table;
            P_Table : constant Pop_Table_Access := new Pop_Table;
            I_Table : constant Installation_Table_Access :=
                        new Installation_Table;
            S_Table : Ship_Table;
            S       : Lui.Tables.Model_Table;

            procedure Add_Market_Row
              (Commodity : Concorde.Commodities.Commodity_Type);

            --------------------
            -- Add_Market_Row --
            --------------------

            procedure Add_Market_Row
              (Commodity : Concorde.Commodities.Commodity_Type)
            is
            begin
               M_Table.Row_Map.Append (Commodity);
            end Add_Market_Row;

         begin
            World.Check_Loaded;
            M_Table.World := World;

            Concorde.Commodities.Db.Scan (Add_Market_Row'Access);

            M_Table.Initialise
              ("Market", M_Table.Row_Map.Last_Index,
               Market_Column'Last);

            M := new Market_Table'(M_Table);

            P_Table.World := World;
            P_Table.Initialise ("Population", 0, Pop_Column'Last);

            I_Table.World := World;
            I_Table.Initialise ("Installations", 0, Installation_Column'Last);

            S_Table.World := World;
            S_Table.Initialise ("Ships", 0, Ship_Column'Last);
            S := new Ship_Table'(S_Table);

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
                 (Lui.Tables.Model_Table (Result.Sector_Properties),
                  M, Lui.Tables.Model_Table (P_Table),
                  Lui.Tables.Model_Table (I_Table), S));

            Result.Pops := P_Table;
            Result.Installations := I_Table;
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
