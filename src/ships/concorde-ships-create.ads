package Concorde.Ships.Create is

   function New_Ship
     (Owner  : not null access constant
        Concorde.Empires.Root_Empire_Type'Class;
      Name   : String;
      World  : in out Concorde.Worlds.Root_World_Type'Class;
      Design : String)
      return Ship_Type;

end Concorde.Ships.Create;
