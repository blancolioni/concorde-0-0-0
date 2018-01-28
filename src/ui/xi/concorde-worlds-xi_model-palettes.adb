with Tropos.Reader;

with WL.String_Maps;

with Concorde.Paths;

package body Concorde.Worlds.Xi_Model.Palettes is

   function Get_Base_Palette
     (World : World_Type)
      return Xi.Color.Xi_Color_1D_Array;

   function Create_Palette
     (Color_Config : Tropos.Configuration)
      return Xi.Color.Xi_Color_1D_Array;

   package Palette_Maps is
     new WL.String_Maps (Xi.Color.Xi_Color_1D_Array, Xi.Color."=");

   Palette_Map : Palette_Maps.Map;

   --------------------
   -- Create_Palette --
   --------------------

   function Create_Palette
     (Color_Config : Tropos.Configuration)
      return Xi.Color.Xi_Color_1D_Array
   is
      use Xi;
      Palette_Config : constant Tropos.Configuration :=
                         Color_Config.Child ("palette");
      Color_Count    : constant Natural := Palette_Config.Child_Count;
      Index          : Natural := 0;

      function To_Xi_Color
        (Config : Tropos.Configuration)
         return Xi.Color.Xi_Color
      is ((Red  => Xi_Float (Float'(Config.Get (1))) / 255.0,
           Green => Xi_Float (Float'(Config.Get (2))) / 255.0,
           Blue  => Xi_Float (Float'(Config.Get (3))) / 255.0,
           Alpha => 1.0));

   begin
      return Result : Xi.Color.Xi_Color_1D_Array (1 .. Color_Count) do
         for Item of Palette_Config loop
            Index := Index + 1;
            Result (Index) := To_Xi_Color (Item);
         end loop;
      end return;
   end Create_Palette;

   ----------------------
   -- Get_Base_Palette --
   ----------------------

   function Get_Base_Palette
     (World : World_Type)
      return Xi.Color.Xi_Color_1D_Array
   is
      Palette_Name   : constant String :=
                         World_Category'Image (World.Category)
                       & "-palette";
   begin
      if not Palette_Map.Contains (Palette_Name) then
         declare
            Palette : constant Xi.Color.Xi_Color_1D_Array :=
                        Create_Palette
                          (Tropos.Reader.Read_Config
                             (Concorde.Paths.Config_File
                                ("palettes/" & Palette_Name & ".txt")));
         begin
            Palette_Map.Insert (Palette_Name, Palette);
            return Palette;
         end;
      else
         return Palette_Map.Element (Palette_Name);
      end if;

   end Get_Base_Palette;

   -----------------
   -- Get_Palette --
   -----------------

   function Get_Palette
     (World : World_Type)
      return Xi.Color.Xi_Color_1D_Array
   is
      use Xi.Color;
      Base : constant Xi_Color_1D_Array :=
               Get_Base_Palette (World);
   begin
      case Rocky_World (World.Category) is
         when Water =>
            return Base;
         when Rock =>
            return Base;
         when Martian =>
            return Base;
         when Venusian =>
            return Base;
         when Ice =>
            return Base;
         when Terrestrial =>
            declare
               Water_Level : constant Natural :=
                               Natural (World.Hydrosphere
                                        * 2.0 * Real (Base'Length));
               Water_Palette : constant Xi_Color_1D_Array (1 .. Water_Level) :=
                                 (others => (0.0, 0.3, 0.6, 1.0));
            begin
               return Water_Palette & Base;
            end;
      end case;
   end Get_Palette;

end Concorde.Worlds.Xi_Model.Palettes;
