private with Ada.Containers.Doubly_Linked_Lists;

with Concorde.Commodities;
with Concorde.Markets;
with Concorde.Quantities;
with Concorde.Trades;
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

   type Trade_State is
     (Bidding, Buying, Asking, Selling, Moving);

   type Root_Ship_Trade_Manager is
     new Root_Ship_Manager with
      record
         Route   : World_Lists.List;
         Current : World_Lists.Cursor;
         State   : Trade_State := Bidding;
      end record;

   function Next_Destination
     (Manager : Root_Ship_Trade_Manager'Class)
      return Concorde.Worlds.World_Type;

   overriding procedure On_Idle
     (Manager : in out Root_Ship_Trade_Manager);

   procedure Create_Asks
     (Manager : in out Root_Ship_Trade_Manager'Class);

   procedure Create_Bids
     (Manager : in out Root_Ship_Trade_Manager'Class);

   procedure Check_Markets
     (Manager            : in out Root_Ship_Trade_Manager'Class;
      Commodity          : Concorde.Commodities.Commodity_Type;
      Current_Market     : Concorde.Markets.Market_Type;
      Destination_Market : Concorde.Markets.Market_Type;
      Space              : in out Concorde.Quantities.Quantity_Type);

end Concorde.Managers.Ships.Trade;
