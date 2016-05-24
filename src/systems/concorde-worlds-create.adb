with Ada.Characters.Handling;
with Ada.Numerics;
with Ada.Text_IO;
with Ada.Long_Float_Text_IO;

with WL.Random;

with Tropos.Reader;

with Memor.Element_Vectors;

with Concorde.Options;
with Concorde.Paths;

with Concorde.Elementary_Functions;
with Concorde.Random;
with Concorde.Roman_Images;

with Concorde.Constants;
with Concorde.Solar_System;

with Concorde.Atmosphere;
with Concorde.Terrain;

--  with Concorde.Terrain.Surface_Maps;

with Concorde.Worlds.Maps;

with Concorde.Worlds.Db;

package body Concorde.Worlds.Create is

   Report_Worlds     : constant Boolean := False;
   pragma Unreferenced (Report_Worlds);

   Verbose_Mode       : constant Boolean := False;

   Write_World_Bitmaps : constant Boolean := True;
   pragma Unreferenced (Write_World_Bitmaps);

   Sector_Size    : Non_Negative_Real := 0.0;
   Subsector_Size : Natural := 0;

   package Terrain_Feature_Vectors is
     new Memor.Element_Vectors (Concorde.Features.Feature_Type, null,
                                Concorde.Features."=");

   type Category_Terrain_Feature_Array is
     array (World_Category) of Terrain_Feature_Vectors.Vector;

   Category_Terrain_Features : Category_Terrain_Feature_Array;
   Got_Category_Terrain_Features : Boolean := False;

   procedure Configure_Category_Terrain;

   procedure Calculate_Climate
     (World : in out Root_World_Type'Class);

   type Orbit_Zone is range 1 .. 3;

   function Get_Orbit_Zone
     (Luminosity   : Real;
      Orbit_Radius : Real)
     return Orbit_Zone;

   function Get_Orbital_Period
     (Separation  : Real;
      Small_Mass  : Real;
      Large_Mass  : Real)
      return Real with Unreferenced;

   procedure Create_Sector_Layout
     (World       : in out Root_World_Type'Class);

   procedure Create_Resources
     (World : in out Root_World_Type'Class);

   function Calculate_Kothari_Radius
     (Earth_Masses : Real;
      Jovian       : Boolean;
      Zone         : Orbit_Zone)
     return Real;
   --  Uses formula from stargen.
   --  Earth_Masses: mass of the World in terms of the mass of Earth
   --  Jovian: true if the World is a gas giant
   --  Zone: the orbit zone (1, 2 or 3; depends on the orbital radius
   --        and the luminosity of the star

   function Calculate_Escape_Velocity
     (Earth_Masses  : Real;
      Earth_Radii   : Real)
     return Real;
   --  mass and radius in terms of earth
   --  result in metres per second

   function Calculate_RMS_Velocity
     (Molecular_Weight : Real;
      Exospheric_Temp  : Real)
     return Real;

   procedure Calculate_Day_Length
     (Star    : in Concorde.Stars.Star_Type;
      World  : in out Root_World_Type'Class);

   function Calculate_Water_Boiling_Point
     (Surface_Pressure : Real)
      return Real;

   procedure Calculate_Gases
     (Star   : Concorde.Stars.Star_Type;
      World  : in out Root_World_Type'Class);

   function Calculate_Opacity
     (Molecular_Weight : Real;
      Surface_Pressure : Real)
      return Real;

   function Calculate_Greenhouse_Rise
     (Optical_Depth    : Real;
      Effective_Temp   : Real;
      Surface_Pressure : Real)
      return Real;

   function Estimated_Temperature
     (Ecosphere_Radius   : Real;
      Orbit_Radius       : Real;
      Albedo             : Real)
     return Real;

   function Effective_Temperature
     (Ecosphere_Radius   : Real;
      Orbit_Radius       : Real;
      Albedo             : Real)
     return Real;

   procedure Calculate_Surface_Temperature
     (Star        : in     Concorde.Stars.Star_Type;
      World      : in out Root_World_Type'Class;
      First       : in     Boolean;
      Ecosphere   : in     Real;
      Last_Water  : in     Real;
      Last_Clouds : in     Real;
      Last_Ice    : in     Real;
      Last_Temp   : in     Real;
      Last_Albedo : in     Real);

   function Calculate_Hydro_Fraction
     (Volatile_Gas_Inventory : Real;
      Earth_Radii            : Real)
      return Real;

   function Calculate_Cloud_Fraction
     (Surface_Temperature  : Real;
      Min_Molecular_Weight : Real;
      Earth_Radii          : Real;
      Hydro_Fraction       : Real)
      return Real;

   function Calculate_Ice_Fraction
     (Hydro_Fraction      : Real;
      Surface_Temperature : Real)
      return Real;

   function Calculate_Albedo
     (Hydro_Fraction : Real;
      Cloud_Fraction : Real;
      Ice_Fraction   : Real;
      Surface_Pressure : Real)
      return Real;

   function Has_Greenhouse
     (Ecosphere_Radius   : Real;
      Orbit_Radius       : Real)
     return Boolean;

   function Molecule_Limit
     (Earth_Masses    : Real;
      Earth_Radii     : Real;
      Exospheric_Temp : Real)
     return Real;

   function Volatile_Inventory
     (Earth_Masses    : Real;
      Escape_Velocity : Real;
      RMS_Velocity    : Real;
      Stellar_Mass    : Real;
      Zone            : Orbit_Zone;
      Greenhouse      : Boolean;
      Accreted_Gas    : Boolean)
     return Real;

   function Volatile_Inventory
     (Star     : Concorde.Stars.Star_Type;
      World   : Root_World_Type'Class)
      return Real;

   function Calculate_Surface_Pressure
     (Volatile_Gas_Inventory : Real;
      Earth_Radii            : Real;
      Earth_Gravities        : Real)
     return Real;

   procedure Iterate_Surface_Temperature
     (Star    : in     Concorde.Stars.Star_Type;
      World  : in out Root_World_Type'Class);

   procedure Set_Temperature_Range
     (World : in out Root_World_Type'Class);

   procedure Set_Axial_Tilt
     (World : in out Root_World_Type'Class);

   function Soft_Limit (V, Max, Min : Real) return Real;

   function Limit (X : Real) return Real;

   ----------------------
   -- Calculate_Albedo --
   ----------------------

   function Calculate_Albedo
     (Hydro_Fraction : Real;
      Cloud_Fraction : Real;
      Ice_Fraction   : Real;
      Surface_Pressure : Real)
      return Real
   is
      use Concorde.Solar_System;
      Rock             : Real := 1.0 - Hydro_Fraction - Ice_Fraction;
      Water            : Real := Hydro_Fraction;
      Ice              : Real := Ice_Fraction;
      Cloud_Adjustment : Real;
      Components       : Real := 0.0;
      Ice_Part, Water_Part, Cloud_Part, Rock_Part : Real;
   begin

      if Water > 0.0 then
         Components := Components + 1.0;
      end if;

      if Ice > 0.0 then
         Components := Components + 1.0;
      end if;

      if Rock > 0.0 then
         Components := Components + 1.0;
      end if;

      Cloud_Adjustment := Cloud_Fraction / Components;

      Rock  := Real'Max (Rock - Cloud_Adjustment, 0.0);
      Water := Real'Max (Water - Cloud_Adjustment, 0.0);
      Ice   := Real'Max (Ice - Cloud_Adjustment, 0.0);

      Cloud_Part := Cloud_Fraction * Cloud_Albedo;

      if Surface_Pressure = 0.0 then
         Rock_Part := Rock * Rocky_Airless_Albedo;
         Ice_Part := Ice * Airless_Ice_Albedo;
         Water_Part := 0.0;
      else
         Rock_Part := Rock * Rocky_Albedo;
         Ice_Part := Ice * Ice_Albedo;
         Water_Part := Water * Water_Albedo;
      end if;

      return Cloud_Part + Ice_Part + Water_Part + Rock_Part;
   end Calculate_Albedo;

   -----------------------
   -- Calculate_Climate --
   -----------------------

   procedure Calculate_Climate
     (World : in out Root_World_Type'Class)
   is
   begin
      for Tile in World.Sectors'Range loop
         declare
            use Concorde.Elementary_Functions;
            Sector : Sector_Record renames World.Sectors (Tile);
            Latitude : constant Real :=
                         World.Surface.Latitude (Tile);
            Pressure : constant Real :=
                         (if World.Surface_Pressure > 0.0
                          then World.Surface_Pressure
                          * Exp (-Real (Height_Range'Max (Sector.Height, 0))
                            * (1.0 / 7.0 / World.Surface_Pressure))
                          else 0.0);
            Height_Factor   : constant Unit_Real :=
                                (if Pressure > 0.0
                                 then Pressure / World.Surface_Pressure
                                 else 1.0);
            Latitude_Factor : constant Real :=
                                80.0 * abs Latitude / 180.0;
         begin
            Sector.Temperature :=
              (Low     =>
                 World.Nighttime_Low * Height_Factor - Latitude_Factor,
               High    =>
                 World.Daytime_High * Height_Factor - Latitude_Factor,
               Average =>
                 (World.Nighttime_Low + World.Daytime_High)
               * Height_Factor / 2.0 - Latitude_Factor);
            if Sector.Height < -4 and then World.Hydrosphere > 0.0 then
               Sector.Terrain :=
                 Concorde.Terrain.Get ("ocean");
               if Sector.Temperature.High < 263.0 then
                  Sector.Feature := Concorde.Features.Ice;
               end if;
            elsif Sector.Height < 0 and then World.Hydrosphere > 0.0 then
               Sector.Terrain :=
                 Concorde.Terrain.Get ("sea");
               if Sector.Temperature.High < 272.0 then
                  Sector.Feature := Concorde.Features.Ice;
               end if;
            elsif World.Ice_Cover > 0.0 or else World.Hydrosphere > 0.0 then
               if Sector.Temperature.High < 276.0 then
                  Sector.Feature := Concorde.Features.Ice;
               end if;
            end if;
         end;
      end loop;
   end Calculate_Climate;

   --     procedure Calculate_Climate
--       (World : in out Root_World_Type'Class)
--     is
--        use Concorde.Elementary_Functions;
--        use Concorde.Terrain;
--        use type Concorde.Features.Feature_Type;
--
--        function Tropic_Curve (Latitude : Positive) return Unit_Real;
--
--        ------------------
--        -- Tropic_Curve --
--        ------------------
--
--        function Tropic_Curve (Latitude : Positive) return Unit_Real is
--        begin
--           if Latitude <= World.Half_Circle_Sectors / 4 then
--              return 0.0;
--           elsif Latitude <= World.Half_Circle_Sectors / 2  - 1 then
--              return 1.0
--                - Real (abs (Latitude - World.Half_Circle_Sectors / 3))
--                / Real (World.Half_Circle_Sectors / 3);
--           elsif Latitude <= World.Half_Circle_Sectors / 2 + 2 then
--              return 0.0;
--           elsif Latitude <= World.Half_Circle_Sectors * 3 / 4 then
--              return 1.0
--                - Real (abs (Latitude - 2 * World.Half_Circle_Sectors / 3))
--                / Real (2 * World.Half_Circle_Sectors / 3);
--           else
--              return 0.0;
--           end if;
--        end Tropic_Curve;
--
--        Last_Water    : Natural;
--        Last_Mountain : Natural;
--        Sector_Index : Natural := World.Sector_Count;
--     begin
--        for Latitude in reverse 1 .. World.Half_Circle_Sectors loop
--
--           Last_Water := World.Row_Length (Latitude) + 1;
--           Last_Mountain := World.Row_Length (Latitude) + 2;
--
--           for Longitude in reverse 1 .. World.Row_Length (Latitude) loop
--              declare
--                 Terrain : constant Terrain_Type :=
--                             World.Sectors (Sector_Index).Terrain;
--                 Tropic_Factor : constant Unit_Real :=
   --  Tropic_Curve (Latitude);
--              begin
--                 if Terrain.Is_Water then
--                    Last_Water := Longitude;
--                 elsif Terrain = Mountain then
--                    Last_Mountain := Longitude;
--                 else
--                    declare
--                       Water_Sectors    : constant Positive :=
--                                            Last_Water - Longitude;
--                       Mountain_Sectors : constant Positive :=
--                                            Last_Mountain - Longitude;
--                       Ocean_Distance   : constant Unit_Real :=
--                                            Sqrt (World.Hydrosphere)
--                                            ** Water_Sectors;
--                       Desert_Chance    : Unit_Real :=
--                                            (1.0 - Ocean_Distance)
--                                            * Tropic_Factor;
--                    begin
--                       if Longitude < World.Row_Length (Latitude)
--                         and then World.Sectors
--                           (Sector_Index + 1).Feature
   --  = Concorde.Features.Desert
--                       then
--                          Desert_Chance := Sqrt (Desert_Chance);
--                       end if;
--
--                       if Mountain_Sectors = 1 then
--                          Desert_Chance := Sqrt (Desert_Chance);
--                       end if;
--
--                       if Last_Water = Longitude + 1 then
--                          Desert_Chance := 0.0;
--                       end if;
--
--                       if Concorde.Random.Unit_Random < Desert_Chance then
--                          World.Sectors (Sector_Index).Feature :=
--                            Concorde.Features.Desert;
--                       end if;
--                    end;
--                 end if;
--              end;
--              Sector_Index := Sector_Index - 1;
--           end loop;
--        end loop;
--     end Calculate_Climate;

   ------------------------------
   -- Calculate_Cloud_Fraction --
   ------------------------------

   function Calculate_Cloud_Fraction
     (Surface_Temperature  : Real;
      Min_Molecular_Weight : Real;
      Earth_Radii          : Real;
      Hydro_Fraction       : Real)
      return Real
   is
   begin

      if Min_Molecular_Weight > 18.0 then
         --  18.0 = molecular weight of water vapour
         return 0.0;
      end if;

      declare
         use Concorde.Elementary_Functions;
         use Concorde.Solar_System;
         Pi : constant := Ada.Numerics.Pi;
         Q2_36 : constant := 0.0698;
         Surface_Area : constant Real :=
                          4.0 * Pi * Earth_Radii ** 2;
         Hydro_Mass   : constant Real :=
                          Hydro_Fraction * Surface_Area
                            * Earth_Water_Mass_Per_Area;
         Water_Vapour_In_Kg : constant Real :=
                                1.0E-8 * Hydro_Mass
                                  * Exp (Q2_36
                                         * (Surface_Temperature
                                             - Earth_Average_Kelvin));
         Fraction           : constant Real :=
                                Cloud_Coverage_Factor
                                  * Water_Vapour_In_Kg
           / Surface_Area;
      begin
         return Real'Min (Fraction, 1.0);
      end;

   end Calculate_Cloud_Fraction;

   --------------------------
   -- Calculate_Day_Length --
   --------------------------

   procedure Calculate_Day_Length
     (Star    : in Concorde.Stars.Star_Type;
      World  : in out Root_World_Type'Class)
   is
      use Concorde.Elementary_Functions;
      use Concorde.Solar_System;
      Pi : constant := Ada.Numerics.Pi;

      World_Mass   : constant Real := World.Mass;
      World_Radius : constant Real := World.Radius;
      World_Year   : constant Real := World.Period;
      Is_Jovian     : constant Boolean :=
                        World.Category in Sub_Jovian .. Jovian;
      J             : constant Real := 1.46e-20;
      K2            : constant Real :=
                        (if Is_Jovian then 0.24 else 0.33);

      Base_Angular_V : constant Real :=
                         Sqrt (2.0 * J * World_Mass
                               / (K2 * World_Radius ** 2));
      Change_In_Angular_V : constant Real := Change_In_Earth_Angular_V
        * (World.Density / Earth_Density)
        * (World_Radius / Earth_Radius)
        * (Earth_Mass / World_Mass)
        * (Star.Mass / Solar_Mass) ** 2
        * (World.Semimajor_Axis / Earth_Orbit) ** (-6.0);

      Angular_V     : constant Real :=
                        Base_Angular_V
                          + Change_In_Angular_V * Star.Age;
      Stopped       : constant Boolean :=
                        Angular_V <= 0.0;
      World_Day    : constant Real :=
                        2.0 * Pi / Angular_V;

   begin

      World.Resonant_Period := False;

      if Stopped or else World_Day > World_Year then

         if World.Eccentricity > 0.1 then
            declare
               E : constant Unit_Real := World.Eccentricity;
               Spin_Resonance_Factor : constant Real :=
                                         (1.0 - E) / (1.0 + E);
            begin
               World.Resonant_Period := True;
               World.Day_Length := Spin_Resonance_Factor * World_Year;
            end;
         else
            World.Day_Length := World_Year;
         end if;

      else

         World.Day_Length := World_Day;

      end if;

   end Calculate_Day_Length;

   -------------------------------
   -- Calculate_Escape_Velocity --
   -------------------------------

   function Calculate_Escape_Velocity
     (Earth_Masses  : Real;
      Earth_Radii   : Real)
     return Real
   is
      use Concorde.Constants;
      use Concorde.Elementary_Functions;
      use Concorde.Solar_System;
   begin
      return Sqrt (2.0 * Gravitational_Constant * Earth_Masses * Earth_Mass
                     / (Earth_Radii * Earth_Radius));
   end Calculate_Escape_Velocity;

   ---------------------
   -- Calculate_Gases --
   ---------------------

   procedure Calculate_Gases
     (Star    : Concorde.Stars.Star_Type;
      World  : in out Root_World_Type'Class)
   is
      use Concorde.Elementary_Functions;
      Pressure : constant Real := World.Surface_Pressure / 1000.0;
      Gases    : constant Concorde.Atmosphere.Array_Of_Gases :=
                   Concorde.Atmosphere.By_Molecular_Weight;
      Count    : Natural := 0;
      YP       : Real;
      MW       : Real;
      Refs     : Concorde.Atmosphere.Array_Of_Gases (Gases'Range);
      Amounts  : array (Refs'Range) of Non_Negative_Real;
      Total    : Real := 0.0;
   begin

      for Gas of Gases loop

         YP :=
           Gas.Boiling_Point
           / 373.0 * (Log (Pressure + 0.001) / (-5050.5) + (1.0 / 373.0));
         MW := Gas.Molecular_Weight;

         if MW >= World.Min_Molecular_Weight
           and then YP >= 0.0
           and then YP < World.Nighttime_Low
         then
            declare
               Formula      : constant String := Gas.Formula;
               VRMS         : constant Real :=
                                Calculate_RMS_Velocity
                                  (MW,
                                   World.Exospheric_Temp);
               PVRMS         : constant Real :=
                                (1.0 / (1.0 +
                                   VRMS / World.Escape_Velocity))
                   ** (Star.Age / 1.0e9);
               Abundance    : constant Real :=
                                Gas.Abundance_S;
               Reactivity   : Real := 1.0;
               Fraction     : Real := 1.0;
               Pressure_2   : Real := 1.0;
               Amount       : Real;
            begin

               if Formula = "Ar" then
                  Reactivity := 0.15 * Star.Age / 4.0e9;
               elsif Formula = "He" then
                  Pressure_2 := 0.75 + Pressure;
                  Reactivity := (1.0 / (1.0 + Gas.Reactivity))
                    ** (Star.Age / 2.0e9 * Pressure_2);
               elsif Formula = "O2"
                 and then Star.Age > 2.0e9
                 and then World.Surface_Temp in 270.0 .. 400.0
               then
                  Pressure_2 := 0.89 + Pressure / 4.0;
                  Reactivity := (1.0 / (1.0 + Gas.Reactivity))
                    ** (((Star.Age / 2.0e9) ** 0.25) * Pressure_2);
               elsif Formula = "CO2"
                 and then Star.Age > 2.0e9
                 and then World.Surface_Temp in 270.0 .. 400.0
               then
                  Pressure_2 := 0.75 + Pressure;
                  Reactivity :=
                    (1.0 / (1.0 + Gas.Reactivity))
                    ** (((Star.Age / 2.0e9) ** 0.5) * Pressure_2);
                  Reactivity := Reactivity * 1.5;
               end if;

               Fraction := (1.0 - (World.Min_Molecular_Weight / MW));
               Amount := Abundance * PVRMS * Reactivity * Fraction;

               if Amount > 0.0 then
                  Count := Count + 1;
                  Refs (Count) := Gas;
                  Amounts (Count) := Amount;
                  Total := Total + Amount;
               end if;

            end;
         end if;

      end loop;

      if Count > 0 then

         for I in 1 .. Count loop
            World.Atmosphere.Append ((Refs (I), Amounts (I) / Total));
         end loop;

         declare
            function Higher_Concentration
              (Left, Right : Atmospheric_Element)
               return Boolean
            is (Left.Fraction > Right.Fraction);

            package Sorting is
              new Atmosphere_Lists.Generic_Sorting
                (Higher_Concentration);
         begin
            Sorting.Sort (World.Atmosphere);
         end;
      end if;

   end Calculate_Gases;

   -------------------------------
   -- Calculate_Greenhouse_Rise --
   -------------------------------

   function Calculate_Greenhouse_Rise
     (Optical_Depth    : Real;
      Effective_Temp   : Real;
      Surface_Pressure : Real)
      return Real
   is
      use Concorde.Elementary_Functions;
      use Concorde.Solar_System;
      Convection_Factor : constant Real :=
          Earth_Convection_Factor
        * (Surface_Pressure / Earth_Surface_Pressure) ** 0.4;
      Rise              : constant Real :=
          ((1.0 + 0.75 * Optical_Depth) ** 0.25 - 1.0)
        * Effective_Temp * Convection_Factor;
   begin

      if Verbose_Mode then
         Ada.Text_IO.Put ("gh: optical depth = ");
         Ada.Long_Float_Text_IO.Put (Optical_Depth, 1, 1, 0);
         Ada.Text_IO.Put ("; eff temp = ");
         Ada.Long_Float_Text_IO.Put (Effective_Temp - 273.0, 1, 1, 0);
         Ada.Text_IO.Put ("; pressure = ");
         Ada.Long_Float_Text_IO.Put (Surface_Pressure, 1, 1, 0);
         Ada.Text_IO.Put ("; rise = ");
         Ada.Long_Float_Text_IO.Put (Rise, 1, 1, 0);
         Ada.Text_IO.New_Line;
      end if;

      return Real'Max (Rise, 0.0);
   end Calculate_Greenhouse_Rise;

   ------------------------------
   -- Calculate_Hydro_Fraction --
   ------------------------------

   function Calculate_Hydro_Fraction
     (Volatile_Gas_Inventory : Real;
      Earth_Radii            : Real)
      return Real
   is
      Result : constant Real :=
                 (0.71 * Volatile_Gas_Inventory / 1000.0)
                 / Earth_Radii ** 2;
   begin
      return Real'Min (Result, 1.0);
   end Calculate_Hydro_Fraction;

   ----------------------------
   -- Calculate_Ice_Fraction --
   ----------------------------

   function Calculate_Ice_Fraction
     (Hydro_Fraction      : Real;
      Surface_Temperature : Real)
      return Real
   is
      use Concorde.Elementary_Functions;
      Result : Real :=
                 (328.0 - Real'Min (Surface_Temperature, 328.0)) / 90.0;
   begin
      Result := Result ** 5;

      Result := Real'Min (Result, 1.5 * Hydro_Fraction);

      return Real'Min (Result, 1.0);

   end Calculate_Ice_Fraction;

   ------------------------------
   -- Calculate_Kothari_Radius --
   ------------------------------

   function Calculate_Kothari_Radius
     (Earth_Masses : Real;
      Jovian       : Boolean;
      Zone         : Orbit_Zone)
     return Real
   is
      use Concorde.Elementary_Functions;
      use Concorde.Solar_System;

      Mass_Ratio : constant Real := 1.0 / Earth_Masses;

      --  Some fudge constants
      --  Taste that delicious, sweet fudge

      A1_20      : constant := 6.485e12;
      A2_20      : constant := 4.0032e-8;
      Beta_20    : constant := 5.71e12;
      Jims_Fudge : constant := 1.09;

      --  method uses cgs system, so here's some conversion constants
      Solar_Mass_In_Grams : constant :=
        Concorde.Solar_System.Solar_Mass * 1000.0;
      Cm_Per_Km : constant := 100.0 * 1000.0;

      Weights : constant array (Boolean, Orbit_Zone) of Real :=
        (False => (15.0, 10.0, 10.0),
         True  => (9.5, 2.47, 7.0));
      Numbers : constant array (Boolean, Orbit_Zone) of Real :=
        (False => (8.0, 5.0, 5.0),
         True  => (4.5, 2.0, 4.0));

      Atomic_Weight : constant Real := Weights (Jovian, Zone);
      Atomic_Number : constant Real := Numbers (Jovian, Zone);

      T_1 : constant Real := Atomic_Weight * Atomic_Number;
      T_2 : constant Real := A2_20 * (Atomic_Weight ** (4.0 / 3.0))
        * (Solar_Mass_In_Grams ** (2.0 / 3.0))
        * (Mass_Ratio ** (2.0 / 3.0))
        / (A1_20 * (Atomic_Number ** 2))
        + 1.0;

      Result : constant Real :=
        2.0 * Beta_20 * (Solar_Mass_In_Grams ** (1.0 / 3.0))
        / (A1_20 * (T_1 ** (1.0 / 3.0)))
        / T_2
        * (Mass_Ratio ** (1.0 / 3.0))
        / Cm_Per_Km
        / Jims_Fudge;
   begin
      return Result;
   end Calculate_Kothari_Radius;

   -----------------------
   -- Calculate_Opacity --
   -----------------------

   function Calculate_Opacity
     (Molecular_Weight : Real;
      Surface_Pressure : Real)
      return Real
   is
      use Concorde.Solar_System;
      Optical_Depth : Real := 0.0;
   begin

      if Molecular_Weight in 0.0 .. 10.0 then
         Optical_Depth := Optical_Depth + 3.0;
      elsif Molecular_Weight in 10.0 .. 20.0 then
         Optical_Depth := Optical_Depth + 2.34;
      elsif Molecular_Weight in 20.0 .. 30.0 then
         Optical_Depth := Optical_Depth + 1.0;
      elsif Molecular_Weight in 30.0 .. 45.0 then
         Optical_Depth := Optical_Depth + 0.15;
      elsif Molecular_Weight in 45.0 .. 100.0 then
         Optical_Depth := Optical_Depth + 0.05;
      end if;

      if Surface_Pressure >= 70.0 * Earth_Surface_Pressure then
         Optical_Depth := Optical_Depth * 8.333;
      elsif Surface_Pressure >= 50.0 * Earth_Surface_Pressure then
         Optical_Depth := Optical_Depth * 6.666;
      elsif Surface_Pressure >= 50.0 * Earth_Surface_Pressure then
         Optical_Depth := Optical_Depth * 3.333;
      elsif Surface_Pressure >= 50.0 * Earth_Surface_Pressure then
         Optical_Depth := Optical_Depth * 2.0;
      elsif Surface_Pressure >= 50.0 * Earth_Surface_Pressure then
         Optical_Depth := Optical_Depth * 1.5;
      end if;

      return Optical_Depth;

   end Calculate_Opacity;

   ----------------------------
   -- Calculate_RMS_Velocity --
   ----------------------------

   function Calculate_RMS_Velocity
     (Molecular_Weight : Real;
      Exospheric_Temp  : Real)
     return Real
   is
      use Concorde.Constants;
      use Concorde.Elementary_Functions;
   begin
      return Sqrt (3.0 * Molar_Gas_Constant
                     * Exospheric_Temp / Molecular_Weight);
   end Calculate_RMS_Velocity;

   --------------------------------
   -- Calculate_Surface_Pressure --
   --------------------------------

   function Calculate_Surface_Pressure
     (Volatile_Gas_Inventory : Real;
      Earth_Radii            : Real;
      Earth_Gravities        : Real)
     return Real
   is
   begin
      return Volatile_Gas_Inventory * Earth_Gravities * 0.1
        / (Earth_Radii ** 2);
   end Calculate_Surface_Pressure;

   -----------------------------------
   -- Calculate_Surface_Temperature --
   -----------------------------------

   procedure Calculate_Surface_Temperature
     (Star        : in     Concorde.Stars.Star_Type;
      World      : in out Root_World_Type'Class;
      First       : in     Boolean;
      Ecosphere   : in     Real;
      Last_Water  : in     Real;
      Last_Clouds : in     Real;
      Last_Ice    : in     Real;
      Last_Temp   : in     Real;
      Last_Albedo : in     Real)
   is
      use Concorde.Constants;
      use Concorde.Solar_System;
      Effective_Temp : Real;
      Greenhouse_Temp : Real;
      Water_Raw       : Real;

      Boil_Off        : Boolean := False;

      Earth_Orbits    : constant Real :=
                          World.Semimajor_Axis / Earth_Orbit;
      Earth_Radii     : constant Real :=
                          World.Radius / Earth_Radius;

   begin

      if First then
         World.Albedo := Concorde.Solar_System.Earth_Albedo;
         Effective_Temp :=
           Effective_Temperature (Ecosphere, Earth_Orbits,
                                  World.Albedo);
         Greenhouse_Temp :=
           Calculate_Greenhouse_Rise
             (Calculate_Opacity
                  (World.Min_Molecular_Weight,
                   World.Surface_Temp),
              Effective_Temp,
              World.Surface_Pressure);

         World.Greenhouse_Rise := Greenhouse_Temp;
         World.Surface_Temp := Effective_Temp + Greenhouse_Temp;

         Set_Temperature_Range (World);

      end if;

      if World.Greenhouse_Effect
        and then World.Max_Temperature < World.Water_Boiling_Point
      then

--           Ada.Text_IO.Put ("Deluge: " & World.Name & " max ");
--           Ada.Long_Float_Text_IO.Put (World.Max_Temperature, 1, 1, 0);
--           Ada.Text_IO.Put (" < boil ");
--           Ada.Long_Float_Text_IO.Put
--             (World.Water_Boiling_Point, 1, 1, 0);
--           Ada.Text_IO.New_Line;

         World.Greenhouse_Effect := False;
         World.Volatile_Gas_Inv :=
           Volatile_Inventory (Star, World);
         World.Surface_Pressure :=
           Calculate_Surface_Pressure
             (World.Volatile_Gas_Inv,
              World.Radius / Earth_Radius,
              World.Surface_Gravity);
         World.Water_Boiling_Point :=
           Calculate_Water_Boiling_Point (World.Surface_Pressure);
      end if;

      Water_Raw := Calculate_Hydro_Fraction (World.Volatile_Gas_Inv,
                                             Earth_Radii);
      World.Hydrosphere := Water_Raw;

      World.Cloud_Cover :=
        Calculate_Cloud_Fraction
          (World.Surface_Temp,
           World.Min_Molecular_Weight,
           Earth_Radii,
           Water_Raw);

      World.Ice_Cover :=
        Calculate_Ice_Fraction
          (Water_Raw,
           World.Surface_Temp);

      if World.Greenhouse_Effect
        and then World.Surface_Pressure > 0.0
      then
         World.Cloud_Cover := 1.0;
      end if;

      if World.Daytime_High >= World.Water_Boiling_Point
        and then not First
        and then (World.Day_Length = World.Period
                  or else World.Resonant_Period)
      then
         World.Hydrosphere := 0.0;
         Boil_Off := True;

         if World.Min_Molecular_Weight > 18.0 then
            --  18.0 = water vapour
            World.Cloud_Cover := 0.0;
         else
            World.Cloud_Cover := 1.0;
         end if;
      end if;

      if World.Surface_Temp
        < Freezing_Point_Of_Water - 3.0
      then
         World.Hydrosphere := 0.0;
      end if;

      World.Albedo :=
        Calculate_Albedo
          (World.Hydrosphere,
           World.Cloud_Cover,
           World.Ice_Cover,
           World.Surface_Pressure);

      Effective_Temp :=
        Effective_Temperature (Ecosphere,
                               World.Semimajor_Axis / Earth_Orbit,
                               World.Albedo);
      Greenhouse_Temp :=
        Calculate_Greenhouse_Rise
          (Calculate_Opacity
             (World.Min_Molecular_Weight,
              World.Surface_Temp),
           Effective_Temp,
           World.Surface_Pressure);

      World.Greenhouse_Rise := Greenhouse_Temp;
      World.Surface_Temp := Effective_Temp + Greenhouse_Temp;

      if not First
        and then not Boil_Off
      then
         World.Hydrosphere :=
           (World.Hydrosphere + Last_Water * 2.0) / 3.0;
         World.Cloud_Cover :=
           (World.Cloud_Cover + Last_Clouds * 2.0) / 3.0;
         World.Ice_Cover :=
           (World.Ice_Cover + Last_Ice * 2.0) / 3.0;
         World.Albedo :=
           (World.Albedo + Last_Albedo * 2.0) / 3.0;
         World.Surface_Temp :=
           (World.Surface_Temp + Last_Temp * 2.0) / 3.0;
      end if;

      Set_Temperature_Range (World);

   end Calculate_Surface_Temperature;

   -----------------------------------
   -- Calculate_Water_Boiling_Point --
   -----------------------------------

   function Calculate_Water_Boiling_Point
     (Surface_Pressure : Real)
      return Real
   is
      use Concorde.Elementary_Functions;
      Surface_Pressure_In_Bars : constant Real :=
                                   Surface_Pressure / 1000.0;
   begin
      return 1.0
        / ((Log (Surface_Pressure_In_Bars) / (-5050.5))
           + (1.0 / 373.0));
   end Calculate_Water_Boiling_Point;

   --------------------------------
   -- Configure_Category_Terrain --
   --------------------------------

   procedure Configure_Category_Terrain is
      Main_Config : constant Tropos.Configuration :=
                      Tropos.Reader.Read_Config
                        (Concorde.Paths.Config_File
                           ("star-systems/world-category.txt"));
   begin
      for Category_Config of Main_Config loop
         declare
            Category : constant World_Category :=
                         World_Category'Value (Category_Config.Config_Name);
         begin
            if Category_Config.Contains ("feature") then
               for Feature_Config of Category_Config.Child ("feature") loop
                  declare
                     Terrain : constant Concorde.Terrain.Terrain_Type :=
                                 Concorde.Terrain.Get
                                   (Feature_Config.Config_Name);
                     Feature : constant Concorde.Features.Feature_Type :=
                                 Concorde.Features.Get
                                   (Feature_Config.Value);
                  begin
                     Category_Terrain_Features (Category).Replace_Element
                       (Terrain.Reference, Feature);
                  end;
               end loop;
            end if;
         end;
      end loop;
   end Configure_Category_Terrain;

   ----------------------
   -- Create_Resources --
   ----------------------

   procedure Create_Resources
     (World : in out Root_World_Type'Class)
   is
      Resources : constant Concorde.Commodities.Array_Of_Commodities :=
                    Concorde.Commodities.Get
                      (Concorde.Commodities.Mineral);
      Local_Res : Concorde.Commodities.Array_Of_Commodities (1 .. 3);
      Probability : array (Local_Res'Range) of Positive;
      Max_Prob    : Natural := 0;
   begin
      for I in Local_Res'Range loop
         Local_Res (I) :=
           Resources (WL.Random.Random_Number (1, Resources'Last));
         Probability (I) := Local_Res'Last - I + 1;
         Max_Prob := Max_Prob + Probability (I);
      end loop;

      for Sector of World.Sectors.all loop
         declare
            function Index return Positive;

            -----------
            -- Index --
            -----------

            function Index return Positive is
               Roll : Natural :=
                        WL.Random.Random_Number (1, Max_Prob);
               Result : Positive := 1;
            begin
               while Roll > Probability (Result) loop
                  Roll := Roll - Probability (Result);
                  Result := Result + 1;
               end loop;
               return Result;
            end Index;

            Resource : constant Concorde.Commodities.Commodity_Type :=
                         Resources (Index);
            Concentration : constant Unit_Real := Concorde.Random.Unit_Random;
            Accessibility : constant Unit_Real := Concorde.Random.Unit_Random;
         begin
            Sector.Deposit :=
              (Resource, Accessibility, Concentration);
         end;
      end loop;
   end Create_Resources;

   --------------------------
   -- Create_Sector_Layout --
   --------------------------

   procedure Create_Sector_Layout
     (World       : in out Root_World_Type'Class)
   is
      use Concorde.Elementary_Functions;
      Seed             : constant Positive :=
                           WL.Random.Random_Number (1, Positive'Last);
   begin

      if not Got_Category_Terrain_Features then
         Configure_Category_Terrain;
         Got_Category_Terrain_Features := True;
      end if;

      if Subsector_Size = 0 then
         Subsector_Size := Concorde.Options.World_Detail_Factor;
      end if;

      declare
         use Ada.Numerics;
         World_Surface_Area : constant Non_Negative_Real :=
                                4.0 / 3.0 * Pi * World.Radius ** 2;
         Sector_Target_Area : constant Non_Negative_Real :=
                                Sector_Size ** 2;
         Sector_Count       : constant Natural :=
                                Natural (World_Surface_Area
                                         / Sector_Target_Area);
         pragma Unreferenced (Sector_Count);
         Normalised_Area    : constant Unit_Real :=
                                Sector_Target_Area / World_Surface_Area;
      begin
         World.Surface :=
           Concorde.Surfaces.Create_Surface (Normalised_Area);
      end;

      World.Sector_Count := Natural (World.Surface.Tile_Count);
      World.Sectors :=
        new Array_Of_Sectors (1 .. World.Surface.Tile_Count);

--        declare
--           Layout : World_Layout_Type :=
--                      (Surface => World.Surface,
--                       Sectors => World.Sectors);
--        begin
      Concorde.Worlds.Maps.Create_World_Map (World);

      World.Surface_Seed := Seed;

      Calculate_Climate (World);

   end Create_Sector_Layout;

   -------------------
   -- Create_Worlds --
   -------------------

   procedure Create_Worlds
     (System : Concorde.Systems.Star_System_Type;
      Star   : Concorde.Stars.Star_Type;
      List   : in out Concorde.Worlds.Lists.List)
   is
      use Concorde.Elementary_Functions;
      use Concorde.Solar_System;

      Rock_Line     : constant Real :=
                        0.3 * Star.Solar_Masses;
      Frost_Line    : constant Real :=
                        2.7 * Star.Solar_Masses;
      Goldilocks_Orbit : constant Non_Negative_Real :=
                           Star.Solar_Masses;
      Have_Goldilocks_World : Boolean := False;

      Current_Orbit : Real;

      World_Index : Positive := 1;

      Width        : Real;
      Density      : Real;
      Solid_Mass   : Real;
      Gas_Mass     : Real;
      Total_Mass   : Real;

      Is_Jovian    : Boolean := False;

      function Random_Accretion_Width
        (Orbit : Non_Negative_Real)
         return Non_Negative_Real;

      function Disk_Density
        (Orbit : Non_Negative_Real)
         return Non_Negative_Real;

      ------------------
      -- Disk_Density --
      ------------------

      function Disk_Density
        (Orbit : Non_Negative_Real)
         return Non_Negative_Real
      is
      begin
         if Orbit < Rock_Line then
            return 0.0;
         elsif Orbit < Frost_Line then
            return Star.Solar_Masses / Orbit / Orbit;
         else
            return (Star.Solar_Masses  +
                      Star.Solar_Masses / Frost_Line * 8.0)
                / Orbit / Orbit;
         end if;
      end Disk_Density;

      ----------------------------
      -- Random_Accretion_Width --
      ----------------------------

      function Random_Accretion_Width
        (Orbit : Non_Negative_Real)
         return Non_Negative_Real
      is
         Min : Real;
         Max : Real;
      begin
         if Orbit < Frost_Line then
            Min := Orbit * 0.25;
            Max := Orbit * 0.65;
         else
            Min := Orbit * 0.25;
            Max := Orbit * 0.75;
         end if;
         return Concorde.Random.Unit_Random * (Max - Min) + Min;
      end Random_Accretion_Width;

   begin

      if Sector_Size = 0.0 then
         Sector_Size :=
           Non_Negative_Real (Concorde.Options.World_Sector_Size)
           * 1000.0;
      end if;

      --        Ada.Text_IO.Put ("Rock line: ");
      --        Ada.Float_Text_IO.Put (Float (Rock_Line), 1, 2, 0);
      --        Ada.Text_IO.New_Line;
      --
      --        Ada.Text_IO.Put ("Frost line: ");
      --        Ada.Float_Text_IO.Put (Float (Frost_Line), 1, 2, 0);
      --        Ada.Text_IO.New_Line;

      Current_Orbit := Rock_Line + Random_Accretion_Width (Rock_Line);

      --        Ada.Text_IO.Put_Line
      --          ("NAME                           " &
      --             "AU       Zone    Mass  Radius       " &
      --      g  Temp  Min MW Pressure");

      loop

         Width := Random_Accretion_Width (Current_Orbit);
         Density := Disk_Density (Current_Orbit);

         exit when Density <= 0.01;

         Solid_Mass :=
           Density * 1.2 * (Width * (Width + 2.0 * Current_Orbit));
         Gas_Mass := 0.0;

         if Solid_Mass > 3.0 then
            --  gas accumulation
            Gas_Mass := Solid_Mass ** 3;
            Is_Jovian := True;
         end if;

         Total_Mass := Solid_Mass + Gas_Mass;

         if not Is_Jovian and then not Have_Goldilocks_World
           and then abs (Current_Orbit / Goldilocks_Orbit - 1.0) < 0.4
           and then abs (Current_Orbit / Goldilocks_Orbit - 1.0) > 0.1
           --  and then Concorde.Random.Unit_Random < 0.5
         then
            Current_Orbit := Concorde.Random.About (Goldilocks_Orbit, 0.1);
         end if;

         Have_Goldilocks_World :=
           Have_Goldilocks_World
             or else abs (Current_Orbit / Goldilocks_Orbit - 1.0) < 0.1;

         declare

            procedure Create
              (World : in out Root_World_Type'Class);

            ------------
            -- Create --
            ------------

            procedure Create
              (World : in out Root_World_Type'Class)
            is
            begin
               World.System := System;
               World.Primary := Star;
               World.Semimajor_Axis := Current_Orbit;
               World.Eccentricity := 0.0;

               World.Set_Name
                 (Star.Name & " "
                  & Concorde.Roman_Images.Roman_Image (World_Index));

               World.Mass := Total_Mass * Earth_Mass;
               World.Semimajor_Axis := Current_Orbit * Earth_Orbit;

               World.Radius :=
                 1000.0 *
                   Calculate_Kothari_Radius
                     (Total_Mass, Is_Jovian,
                      Get_Orbit_Zone (Star.Luminosity, Current_Orbit));
               World.Density :=
                 Ada.Numerics.Pi * World.Radius ** 3 / World.Mass;

               World.Solid_Mass := Solid_Mass * Earth_Mass;
               World.Gas_Mass := Gas_Mass * Earth_Mass;

               Set_Axial_Tilt (World);

               World.Escape_Velocity :=
                 Calculate_Escape_Velocity
                   (Total_Mass,
                    World.Radius / Earth_Radius);

               World.Surface_Acceleration :=
                 Concorde.Constants.Gravitational_Constant
                   * World.Mass / (World.Radius ** 2);

               World.Surface_Gravity :=
                 World.Surface_Acceleration
                   / Concorde.Solar_System.Earth_Gravity;

               World.Exospheric_Temp :=
                 Earth_Exospheric_Temp
                   / ((Current_Orbit / Sqrt (Star.Luminosity)) ** 2);

               Calculate_Day_Length (Star, World);

               if Is_Jovian then

                  World.Greenhouse_Effect := False;
                  World.Volatile_Gas_Inv := Real'Last;
                  World.Surface_Pressure := Real'Last;
                  World.Water_Boiling_Point := Real'Last;
                  World.Surface_Temp := Real'Last;
                  World.Greenhouse_Rise := 0.0;
                  World.Albedo := Gas_Giant_Albedo;
                  World.Hydrosphere := 1.0;
                  World.Cloud_Cover := 1.0;
                  World.Ice_Cover := 0.0;

                  World.Surface_Temp :=
                    Estimated_Temperature
                      (Star.Ecosphere, Current_Orbit,
                       World.Albedo);

               else
                  World.RMS_Velocity :=
                    Calculate_RMS_Velocity (14.0, World.Exospheric_Temp);

                  World.Min_Molecular_Weight :=
                    Molecule_Limit
                      (Total_Mass,
                       World.Radius / Earth_Radius,
                       World.Exospheric_Temp);

                  World.Greenhouse_Effect :=
                    Has_Greenhouse (Star.Ecosphere, Current_Orbit);

                  World.Volatile_Gas_Inv :=
                    Volatile_Inventory
                      (Total_Mass,
                       World.Escape_Velocity,
                       World.RMS_Velocity,
                       Star.Solar_Masses,
                       Get_Orbit_Zone
                         (Star.Luminosity, Current_Orbit),
                       World.Greenhouse_Effect,
                       False);

                  World.Surface_Pressure :=
                    Calculate_Surface_Pressure
                      (World.Volatile_Gas_Inv,
                       World.Radius / Earth_Radius,
                       World.Surface_Gravity);

                  World.Albedo := Earth_Albedo;

                  if World.Surface_Pressure = 0.0 then
                     World.Water_Boiling_Point := 0.0;
                  else
                     World.Water_Boiling_Point :=
                       Calculate_Water_Boiling_Point
                         (World.Surface_Pressure);
                  end if;

                  Iterate_Surface_Temperature (Star, World);

                  if World.Max_Temperature
                    >= Concorde.Constants.Freezing_Point_Of_Water
                    and then World.Min_Temperature
                      <= World.Water_Boiling_Point
                    and then World.Surface_Pressure > 0.0
                  then
                     Calculate_Gases (Star, World);
                  end if;

               end if;

               declare
                  Pressure : constant Non_Negative_Real :=
                               World.Surface_Pressure;
                  Category : World_Category;
               begin
                  if Is_Jovian then
                     Category := Jovian;
                  elsif Pressure < 1.0 then
                     Category := Rock;
                  elsif Pressure > 6000.0
                    and then World.Min_Molecular_Weight <= 2.0
                  then
                     Category := Sub_Jovian;
                  else
                     if World.Hydrosphere >= 0.95 then
                        Category := Water;
                     elsif World.Ice_Cover >= 0.95 then
                        Category := Ice;
                     elsif World.Hydrosphere >= 0.05 then
                        Category := Terrestrial;
                     elsif World.Max_Temperature
                       >= World.Water_Boiling_Point
                     then
                        Category := Venusian;
                     elsif Pressure < 250.0 then
                        Category := Martian;
                     elsif World.Surface_Temp
                       < Concorde.Constants.Freezing_Point_Of_Water
                     then
                        Category := Terrestrial;
                     else
                        Ada.Text_IO.Put_Line
                          (World.Name & ": can't categorise");
                        Category := Rock;
                     end if;
                  end if;

                  World.Category := Category;

                  declare
                     function To_Lower
                       (S : String) return String
                        renames Ada.Characters.Handling.To_Lower;
                  begin
                     Ada.Text_IO.Put
                       (" " & To_Lower (World_Category'Image (Category)));
                  end;

                  if World.Category /= Jovian
                    and then World.Category /= Sub_Jovian
                  then
                     Create_Sector_Layout
                       (World);
                  end if;
               end;

               if World.Category /= Jovian
                 and then World.Category /= Sub_Jovian
               then
                  Create_Resources (World);
               end if;

            end Create;

            New_World : constant World_Type :=
                          Concorde.Worlds.Db.Create (Create'Access);
         begin
            List.Append (New_World);
            World_Index := World_Index + 1;
         end;

         Current_Orbit := Current_Orbit + Width * 2.0;

      end loop;

      Ada.Text_IO.Put_Line (World_Index'Img & " worlds");

   end Create_Worlds;

   ----------------------------
   --  Effective_Temperature --
   ----------------------------

   function Effective_Temperature
     (Ecosphere_Radius   : Real;
      Orbit_Radius       : Real;
      Albedo             : Real)
     return Real
   is
      use Concorde.Elementary_Functions;
      use Concorde.Solar_System;
   begin
      return Sqrt (Ecosphere_Radius / Orbit_Radius)
        * ((1.0 - Albedo) / (1.0 - Earth_Albedo)) ** 0.25
        * Earth_Effective_Temp;
   end Effective_Temperature;

   ---------------------------
   -- Estimated_Temperature --
   ---------------------------

   function Estimated_Temperature
     (Ecosphere_Radius   : Real;
      Orbit_Radius       : Real;
      Albedo             : Real)
     return Real
   is
      use Concorde.Elementary_Functions;
      use Concorde.Solar_System;
   begin
      return Sqrt (Ecosphere_Radius / Orbit_Radius)
        * ((1.0 - Albedo) / (1.0 - Earth_Albedo)) ** 0.25
        * Earth_Average_Kelvin;
   end Estimated_Temperature;

   --------------------
   -- Get_Orbit_Zone --
   --------------------

   function Get_Orbit_Zone
     (Luminosity   : Real;
      Orbit_Radius : Real)
     return Orbit_Zone
   is
      use Concorde.Elementary_Functions;
      Sqrt_Lum : constant Real := Sqrt (Luminosity);
   begin
      if Orbit_Radius < 4.0 * Sqrt_Lum then
         return 1;
      elsif Orbit_Radius < 15.0 * Sqrt_Lum then
         return 2;
      else
         return 3;
      end if;
   end Get_Orbit_Zone;

   ------------------------
   -- Get_Orbital_Period --
   ------------------------

   function Get_Orbital_Period
     (Separation  : Real;
      Small_Mass  : Real;
      Large_Mass  : Real)
      return Real
   is
      use Concorde.Elementary_Functions;
      use Concorde.Solar_System;
      AUs : constant Real := Separation / Earth_Orbit;
      Small : constant Real := Small_Mass / Solar_Mass;
      Large : constant Real := Large_Mass / Solar_Mass;
      Years : constant Real :=
                Sqrt (AUs ** 3 / (Small + Large));
   begin
      return Years * Earth_Sidereal_Year * 24.0 * 3600.0;
   end Get_Orbital_Period;

   --------------------
   -- Has_Greenhouse --
   --------------------

   function Has_Greenhouse
     (Ecosphere_Radius   : Real;
      Orbit_Radius       : Real)
     return Boolean
   is
      Temp : constant Real :=
        Effective_Temperature
        (Ecosphere_Radius, Orbit_Radius,
         Concorde.Constants.Greenhouse_Trigger_Albedo);
   begin
      return Temp > Concorde.Constants.Freezing_Point_Of_Water;
   end Has_Greenhouse;

   ---------------------------------
   -- Iterate_Surface_Temperature --
   ---------------------------------

   procedure Iterate_Surface_Temperature
     (Star    : in     Concorde.Stars.Star_Type;
      World  : in out Root_World_Type'Class)
   is
      use Concorde.Solar_System;
--        Initial_Temp : Real :=
--                         Estimated_Temperature
--                           (Star.Ecosphere,
--                            World.Semimajor_Axis / Earth_Orbit,
--                            World.Albedo);
--        H2, H2O, N2, N : Concorde.Db.Atm_Gas.Atm_Gas_Type;

   begin

      Calculate_Surface_Temperature
        (Star, World, True, Star.Ecosphere,
         0.0, 0.0, 0.0, 0.0, 0.0);

      for I in 1 .. 25 loop

         declare
            Last_Water  : constant Real := World.Hydrosphere;
            Last_Clouds : constant Real := World.Cloud_Cover;
            Last_Ice    : constant Real := World.Ice_Cover;
            Last_Temp   : constant Real := World.Surface_Temp;
            Last_Albedo : constant Real := World.Albedo;
         begin

            Calculate_Surface_Temperature
              (Star        => Star,
               World      => World,
               First       => False,
               Ecosphere   => Star.Ecosphere,
               Last_Water  => Last_Water,
               Last_Clouds => Last_Clouds,
               Last_Ice    => Last_Ice,
               Last_Temp   => Last_Temp,
               Last_Albedo => Last_Albedo);

            exit when abs (World.Surface_Temp - Last_Temp) < 0.25;
         end;

      end loop;

--        H2.By_Formula (Star.Handle, "H2");
--        H2O.By_Formula (Star.Handle, "H2O");
--        N2.By_Formula (Star.Handle, "N2");
--        N.By_Formula (Star.Handle, "N");

--        declare
--           H2_Life  : constant Real := Calculate_Gas_Life (World, H2);
--           H2O_Life : constant Real := Calculate_Gas_Life (World, H2O);
--           N2_Life  : constant Real := Calculate_Gas_Life (World, N2);
--           N_Life   : constant Real := Calculate_Gas_Life (World, N);
--        begin
--
--           null;
--        end;

   end Iterate_Surface_Temperature;

   -----------
   -- Limit --
   -----------

   function Limit (X : Real) return Real is
      use Concorde.Elementary_Functions;
   begin
      return X / Sqrt (Sqrt (1.0 + X * X * X * X));
   end Limit;

   --------------------
   -- Molecule_Limit --
   --------------------

   function Molecule_Limit
     (Earth_Masses    : Real;
      Earth_Radii     : Real;
      Exospheric_Temp : Real)
     return Real
   is
      use Concorde.Constants;
      Gas_Retention_Threshold : constant := 6.0;
      Escape_Velocity         : constant Real :=
        Calculate_Escape_Velocity (Earth_Masses, Earth_Radii);
   begin
      return (3.0 * Molar_Gas_Constant * Exospheric_Temp)
        / (Escape_Velocity / Gas_Retention_Threshold) ** 2;
   end Molecule_Limit;

   --------------------
   -- Set_Axial_Tilt --
   --------------------

   procedure Set_Axial_Tilt
     (World : in out Root_World_Type'Class)
   is
   begin
      World.Tilt := Concorde.Random.Unit_Random * 40.0;
   end Set_Axial_Tilt;

   ---------------------------
   -- Set_Temperature_Range --
   ---------------------------

   procedure Set_Temperature_Range
     (World : in out Root_World_Type'Class)
   is
      use Concorde.Elementary_Functions;
      Pressure_Mod : constant Real :=
          1.0 / Sqrt (1.0 + 20.0 * World.Surface_Pressure / 1000.0);
      PP_Mod       : constant Real :=
          1.0 / Sqrt (10.0 + 5.0 * World.Surface_Pressure / 1000.0);
      Tilt_Mod     : constant Real :=
                       abs (Cos (World.Tilt, 360.0)
                            * (1.0 + World.Eccentricity) ** 2);
      Day_Mod      : constant Real :=
                       1.0 / (200.0 * 3600.0 / World.Day_Length + 1.0);
      MH           : constant Real := (1.0 + Day_Mod) ** Pressure_Mod;
      ML           : constant Real := (1.0 - Day_Mod) ** Pressure_Mod;
      Hi           : constant Real := MH * World.Surface_Temp;
      Max          : constant Real :=
                       World.Surface_Temp
                         + Sqrt (World.Surface_Temp) * 10.0;
      Min          : constant Real :=
                       World.Surface_Temp
                         / Sqrt (World.Day_Length / 3600.0 + 24.0);
      Lo           : constant Real :=
                       Real'Max (ML * World.Surface_Temp, Min);
      SH           : constant Real :=
                       Hi + ((100.0 + Hi) * Tilt_Mod) ** Sqrt (PP_Mod);
      WL           : constant Real :=
                       Real'Max (Lo
                                 - ((150.0 + Lo) * Tilt_Mod) ** Sqrt (PP_Mod),
                                 0.0);
   begin
      World.Daytime_High := Soft_Limit (Hi, Max, Min);
      World.Nighttime_Low := Soft_Limit (Lo, Max, Min);
      World.Max_Temperature := Soft_Limit (SH, Max, Min);
      World.Min_Temperature := Soft_Limit (WL, Max, Min);
   end Set_Temperature_Range;

   ----------------
   -- Soft_Limit --
   ----------------

   function Soft_Limit (V, Max, Min : Real) return Real is
      DV : constant Real := V - Min;
      DM : constant Real := Max - Min;
   begin
      return (Limit (2.0 * DV / DM - 1.0) + 1.0) / 2.0 * DM + Min;
   end Soft_Limit;

   ------------------------
   -- Volatile_Inventory --
   ------------------------

   function Volatile_Inventory
     (Earth_Masses    : Real;
      Escape_Velocity : Real;
      RMS_Velocity    : Real;
      Stellar_Mass    : Real;
      Zone            : Orbit_Zone;
      Greenhouse      : Boolean;
      Accreted_Gas    : Boolean)
     return Real
   is
      use Concorde.Constants;
      use Concorde.Solar_System;
      Velocity_Ratio : constant Real :=
        Escape_Velocity / RMS_Velocity;
      Proportion : Real;
      Result     : Real;
   begin
      if Velocity_Ratio >= Gas_Retention_Threshold then

         case Zone is
            when 1 =>
               Proportion := 140_000.0;
            when 2 =>
               Proportion := 75_000.0;
            when 3 =>
               Proportion := 250.0;
         end case;

         Result := Proportion * Earth_Masses / Stellar_Mass;
         Result := Concorde.Random.About (Result, 0.2);
         if Greenhouse or else Accreted_Gas then
            return Result;
         else
            return Result / 140.0;
         end if;

      else
         return 0.0;
      end if;
   end Volatile_Inventory;

   ------------------------
   -- Volatile_Inventory --
   ------------------------

   function Volatile_Inventory
     (Star     : Concorde.Stars.Star_Type;
      World   : Root_World_Type'Class)
      return Real
   is
      use Concorde.Solar_System;
   begin
      return Volatile_Inventory
        (Earth_Masses    => World.Mass / Earth_Mass,
         Escape_Velocity => World.Escape_Velocity,
         RMS_Velocity    => World.RMS_Velocity,
         Stellar_Mass    => Star.Mass / Solar_Mass,
         Zone            =>
           Get_Orbit_Zone
             (Star.Luminosity,
              World.Semimajor_Axis / Earth_Orbit),
         Greenhouse      => World.Greenhouse_Effect,
         Accreted_Gas    => False);
   end Volatile_Inventory;

end Concorde.Worlds.Create;
