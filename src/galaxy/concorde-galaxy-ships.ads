with Concorde.Ships;

package Concorde.Galaxy.Ships is

   procedure Start_Ship_Moves;

   procedure Move_Ship
     (Ship : Concorde.Ships.Ship_Type);

   procedure Commit_Ship_Moves;

   function Can_Move_To
     (Ship : Concorde.Ships.Ship_Type;
      Destination : Concorde.Systems.Star_System_Type)
      return Boolean;

end Concorde.Galaxy.Ships;
