with Concorde.Worlds;

with Concorde.Ships.Db;

package body Concorde.Ships.Invariants is

   ----------------------
   -- Check_Invariants --
   ----------------------

   procedure Check_Invariants is

      procedure Check (Ship : Ship_Type);

      -----------
      -- Check --
      -----------

      procedure Check (Ship : Ship_Type) is
      begin
         pragma Assert
           (not Ship.Has_Destination
            or else Ship.Orbiting (Ship.Destination),
            Ship.Short_Description
            & " has current system as destination");
      end Check;

   begin
      Concorde.Ships.Db.Scan (Check'Access);
   end Check_Invariants;

end Concorde.Ships.Invariants;
