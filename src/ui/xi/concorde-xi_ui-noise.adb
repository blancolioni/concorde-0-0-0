with Tropos.Reader;

with Xi.Color;

with WL.String_Maps;

with Concorde.Paths;

package body Concorde.Xi_UI.Noise is

   function Create_Palette
     (Color_Config : Tropos.Configuration)
      return Xi.Color.Xi_Color_1D_Array;

   package Palette_Maps is
     new WL.String_Maps (Xi.Color.Xi_Color_1D_Array, Xi.Color."=");

   Palette_Map : Palette_Maps.Map;

   function Get_Palette (Name : String) return Xi.Color.Xi_Color_1D_Array;

   -------------------------
   -- Create_Noise_Shader --
   -------------------------

   function Create_Noise_Shader
     (Palette_Name : String;
      Initiator    : Integer)
      return Xi.Shader.Noise.Xi_Noise_Shader
   is
   begin
      return Xi.Shader.Noise.Create_Noise_Shader
        (Initiator  => Initiator,
         Octaves    => 2.0,
         Roughness  => 0.5,
         Lacunarity => 2.0,
         Palette    => Get_Palette (Palette_Name));
   end Create_Noise_Shader;

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

   -----------------
   -- Get_Palette --
   -----------------

   function Get_Palette (Name : String) return Xi.Color.Xi_Color_1D_Array is
   begin
      if not Palette_Map.Contains (Name) then
         declare
            Palette : constant Xi.Color.Xi_Color_1D_Array :=
                     Create_Palette
           (Tropos.Reader.Read_Config
              (Concorde.Paths.Config_File
                 ("palettes/" & Name & ".txt")));
         begin
            Palette_Map.Insert (Name, Palette);
            return Palette;
         end;
      else
         return Palette_Map.Element (Name);
      end if;

   end Get_Palette;

end Concorde.Xi_UI.Noise;
