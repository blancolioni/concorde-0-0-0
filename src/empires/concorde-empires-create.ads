with Concorde.AI;

package Concorde.Empires.Create is

   procedure New_Empire
     (Name                : String;
      Capital             : String;
      Colour              : Lui.Colours.Colour_Type;
      Default_Ship_Design : String;
      AI                  : Concorde.AI.AI_Type      := null);

end Concorde.Empires.Create;
