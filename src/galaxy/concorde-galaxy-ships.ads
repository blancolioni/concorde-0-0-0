with Concorde.Ships;

package Concorde.Galaxy.Ships is

   procedure Start_Ship_Moves;

   procedure Move_Ship
     (Ship : in out Concorde.Ships.Root_Ship_Type'Class)
     with Pre => Ship.Has_Destination;

   procedure Commit_Ship_Moves;

   function Can_Move_To
     (Ship : Concorde.Ships.Root_Ship_Type'Class;
      Destination : Concorde.Systems.Star_System_Type)
      return Boolean;

end Concorde.Galaxy.Ships;
