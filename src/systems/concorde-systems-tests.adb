with Concorde.Ships.Create;

package body Concorde.Systems.Tests is

   -------------------
   -- New_Test_Ship --
   -------------------

   procedure New_Test_Ship
     (Owner  : not null access constant
        Concorde.Empires.Root_Empire_Type'Class;
      System : in out Concorde.Systems.Root_Star_System_Type'Class;
      Design : String)
   is
      Ship : constant Concorde.Ships.Ship_Type :=
               Concorde.Ships.Create.New_Ship
                 (Owner, System, Design);
   begin
      System.Ships.Append (Ship);
   end New_Test_Ship;

end Concorde.Systems.Tests;
