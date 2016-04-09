package Concorde.Ships.Create is

   function New_Ship
     (Owner  : not null access constant
        Concorde.Empires.Root_Empire_Type'Class;
      System : in out Concorde.Systems.Root_Star_System_Type'Class;
      Design : String)
      return Ship_Type;

end Concorde.Ships.Create;
