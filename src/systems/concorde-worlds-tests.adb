with Concorde.Ships.Create;

package body Concorde.Worlds.Tests is

   -------------------
   -- New_Test_Ship --
   -------------------

   procedure New_Test_Ship
     (Owner  : not null access constant
        Concorde.Empires.Root_Empire_Type'Class;
      World  : in out Root_World_Type'Class;
      Design : String)
   is
      Ship : constant Concorde.Ships.Ship_Type :=
               Concorde.Ships.Create.New_Ship
                 (Owner, "", Db.Reference (World.Reference), Design);
   begin
      World.Ships.Append (Ship);
   end New_Test_Ship;

end Concorde.Worlds.Tests;
