with Concorde.Empires;
with Concorde.Ships;

package Concorde.Worlds.Tests is

   procedure New_Test_Ship
     (Owner  : not null access constant
        Concorde.Empires.Root_Empire_Type'Class;
      World  : in out Root_World_Type'Class;
      Design : String);

end Concorde.Worlds.Tests;
