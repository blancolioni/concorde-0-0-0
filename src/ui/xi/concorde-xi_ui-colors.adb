package body Concorde.Xi_UI.Colors is

   --------------------
   -- Set_Lui_Color --
   --------------------

   procedure Set_Lui_Color
     (Item   : not null access Xi.Color.Xi_Has_Color'Class;
      Color : Lui.Colors.Color_Type)
   is
      use Xi;
   begin
      Item.Set_Color
        (Xi_Float (Color.Red), Xi_Float (Color.Green),
         Xi_Float (Color.Blue), Xi_Float (Color.Alpha));
   end Set_Lui_Color;

   -----------------
   -- To_Xi_Color --
   -----------------

   function To_Xi_Color
     (Color : Lui.Colors.Color_Type)
      return Xi.Color.Xi_Color
   is
      use Xi;
   begin
      return (Xi_Float (Color.Red), Xi_Float (Color.Green),
              Xi_Float (Color.Blue), Xi_Float (Color.Alpha));
   end To_Xi_Color;

end Concorde.Xi_UI.Colors;
