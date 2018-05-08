with Concorde.Ships;

package Concorde.Galaxy.Ships is

   procedure Start_Ship_Moves;

   procedure Move_Ship
     (Ship : Concorde.Ships.Ship_Type)
     with Pre => Ship.Has_Destination;

   procedure Commit_Ship_Moves;

   function Can_Move_To
     (Ship : Concorde.Ships.Root_Vessel_Type'Class;
      Destination : Concorde.Systems.Star_System_Type)
      return Boolean;

end Concorde.Galaxy.Ships;
