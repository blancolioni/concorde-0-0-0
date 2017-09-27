private with Ada.Containers.Doubly_Linked_Lists;

with Concorde.Worlds;

package Concorde.Managers.Ships.Trade is

   type Root_Ship_Trade_Manager is
     new Root_Ship_Manager with private;

   procedure Add_Waypoint
     (Manager : in out Root_Ship_Trade_Manager'Class;
      World   : Concorde.Worlds.World_Type);

   procedure Delete_Waypoint
     (Manager : in out Root_Ship_Trade_Manager'Class;
      World   : Concorde.Worlds.World_Type);

   type Ship_Trade_Manager is access all Root_Ship_Trade_Manager'Class;

   function Create_Manager
     (Ship    : Concorde.Ships.Ship_Type;
      Start   : Concorde.Worlds.World_Type)
      return Ship_Trade_Manager;

   function Create_Manager
     (Ship     : Concorde.Ships.Ship_Type;
      From, To : Concorde.Worlds.World_Type)
      return Ship_Trade_Manager;

private

   package World_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Concorde.Worlds.World_Type, Concorde.Worlds."=");

   type Root_Ship_Trade_Manager is
     new Root_Ship_Manager with
      record
         Route : World_Lists.List;
      end record;

   overriding procedure On_Activated
     (Manager : in out Root_Ship_Trade_Manager;
      Time    : Concorde.Dates.Date_Type);

   overriding procedure On_Idle
     (Manager : in out Root_Ship_Trade_Manager);

end Concorde.Managers.Ships.Trade;
