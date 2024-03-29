with Concorde.Orders.Ships;

package body Concorde.Players is

   --------------------
   -- Execute_Orders --
   --------------------

   procedure Execute_Orders
     (Player : in out Root_Player_Type'Class)
   is
   begin
      for Order of Player.Orders loop
         Order.Execute;
      end loop;
      Player.Orders.Clear;
   end Execute_Orders;

   ------------------------
   -- Order_Colonisation --
   ------------------------

   procedure Order_Colonisation
     (Player      : in out Root_Player_Type'Class;
      Empire      : not null access constant
        Concorde.Empires.Root_Empire_Type'Class;
      Ship        : Concorde.Ships.Ship_Type)
   is
   begin
      Player.Orders.Append
        (Concorde.Orders.Ships.Colonise_Order
           (Concorde.Empires.Empire_Type (Empire), Ship));
   end Order_Colonisation;

   --------------------------
   -- Order_Explore_System --
   --------------------------

   procedure Order_Explore_System
     (Player      : in out Root_Player_Type'Class;
      Empire      : Concorde.Empires.Empire_Type;
      Ship        : Concorde.Ships.Ship_Type;
      Destination : Concorde.Systems.Star_System_Type)
   is
   begin
      Player.Orders.Append
        (Concorde.Orders.Ships.Move_Ship_To_System
           (Empire, Ship, Destination));
   end Order_Explore_System;

   ---------------------
   -- Order_Move_Ship --
   ---------------------

   procedure Order_Move_Ship
     (Player      : in out Root_Player_Type'Class;
      Empire      : Concorde.Empires.Empire_Type;
      Ship        : Concorde.Ships.Ship_Type;
      Destination : Concorde.Worlds.World_Type)
   is
   begin
      Player.Orders.Append
        (Concorde.Orders.Ships.Move_Ship_Order
           (Empire, Ship, Destination));
   end Order_Move_Ship;

end Concorde.Players;
