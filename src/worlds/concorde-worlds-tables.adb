package body Concorde.Worlds.Tables is

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

   -------------------
   -- Height_Colour --
   -------------------

   function Height_Colour
     (Height : Height_Range)
      return Lui.Colours.Colour_Type
   is
   begin
      return Colour : Lui.Colours.Colour_Type do
         Colour := Lui.Colours.To_Colour
           (Height_Red (Height),
            Height_Green (Height),
            Height_Blue (Height));
      end return;

   end Height_Colour;

   ------------------------
   -- Temperature_Colour --
   ------------------------

   function Temperature_Colour
     (Temperature : Non_Negative_Real)
      return Lui.Colours.Colour_Type
   is
      Int_Temp    : constant Integer :=
                      Integer'Max
                        (Temperature_Palette'First (1),
                         Integer'Min
                           (Temperature_Palette'Last (1),
                            Integer (Temperature)));
      Temp_Colour : constant Lui.Colours.Colour_Type :=
                      Lui.Colours.To_Colour
                        (Temperature_Palette (Int_Temp, 1),
                         Temperature_Palette (Int_Temp, 2),
                         Temperature_Palette (Int_Temp, 3));
   begin
      return Temp_Colour;
   end Temperature_Colour;

end Concorde.Worlds.Tables;
