with Concorde.Empires;
with Concorde.Ships;

package Concorde.Systems.Tests is

   procedure New_Test_Ship
     (Owner  : not null access constant
        Concorde.Empires.Root_Empire_Type'Class;
      System : in out Concorde.Systems.Root_Star_System_Type'Class;
      Design : String);

end Concorde.Systems.Tests;
