with Lui.Colors;

with Xi.Color;

package Concorde.Xi_UI.Colors is

   procedure Set_Lui_Color
     (Item   : not null access Xi.Color.Xi_Has_Color'Class;
      Color : Lui.Colors.Color_Type);

   function To_Xi_Color
     (Color : Lui.Colors.Color_Type)
      return Xi.Color.Xi_Color;

end Concorde.Xi_UI.Colors;
