with Concorde.Players;

package Concorde.Empires.Create is

   procedure New_Empire
     (Name                : String;
      Capital             : String;
      Colour              : Lui.Colours.Colour_Type;
      Default_Ship_Design : String;
      Player              : Concorde.Players.Player_Type);

end Concorde.Empires.Create;
