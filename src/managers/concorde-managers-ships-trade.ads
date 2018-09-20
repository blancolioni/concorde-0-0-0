private with Ada.Containers.Doubly_Linked_Lists;

with Concorde.Commodities;
with Concorde.Trades;

with Concorde.People.Communities;

package Concorde.Managers.Ships.Trade is

   type Root_Ship_Trade_Manager is
     new Root_Ship_Manager with private;

   type Ship_Trade_Manager is access all Root_Ship_Trade_Manager'Class;

   function Create_Manager
     (Ship    : Concorde.Ships.Ship_Type;
      Start   : Concorde.People.Communities.Community_Type)
      return Ship_Trade_Manager;

private

   package World_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Concorde.Worlds.World_Type, Concorde.Worlds."=");

   type Trade_State is
     (Bidding, Buying, Asking, Selling, Moving);

   type Root_Ship_Trade_Manager is
     new Root_Ship_Manager with
      record
         State             : Trade_State := Bidding;
         Next_Destination  : Concorde.People.Communities.Community_Type;
      end record;

   overriding function Description
     (Manager : Root_Ship_Trade_Manager)
      return String
   is ("Trade manager for " & Manager.Ship.Name);

   overriding procedure On_Idle
     (Manager : in out Root_Ship_Trade_Manager);

end Concorde.Managers.Ships.Trade;
