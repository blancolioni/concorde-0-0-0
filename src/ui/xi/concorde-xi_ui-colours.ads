with Lui.Colours;

with Xi.Color;

package Concorde.Xi_UI.Colours is

   procedure Set_Lui_Colour
     (Item   : not null access Xi.Color.Xi_Has_Color'Class;
      Colour : Lui.Colours.Colour_Type);

   function To_Xi_Color
     (Colour : Lui.Colours.Colour_Type)
      return Xi.Color.Xi_Color;

end Concorde.Xi_UI.Colours;
