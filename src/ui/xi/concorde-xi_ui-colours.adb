package body Concorde.Xi_UI.Colours is

   --------------------
   -- Set_Lui_Colour --
   --------------------

   procedure Set_Lui_Colour
     (Item   : not null access Xi.Color.Xi_Has_Color'Class;
      Colour : Lui.Colours.Colour_Type)
   is
      use Xi;
   begin
      Item.Set_Color
        (Xi_Float (Colour.Red), Xi_Float (Colour.Green),
         Xi_Float (Colour.Blue), Xi_Float (Colour.Alpha));
   end Set_Lui_Colour;

   -----------------
   -- To_Xi_Color --
   -----------------

   function To_Xi_Color
     (Colour : Lui.Colours.Colour_Type)
      return Xi.Color.Xi_Color
   is
      use Xi;
   begin
      return (Xi_Float (Colour.Red), Xi_Float (Colour.Green),
              Xi_Float (Colour.Blue), Xi_Float (Colour.Alpha));
   end To_Xi_Color;

end Concorde.Xi_UI.Colours;
