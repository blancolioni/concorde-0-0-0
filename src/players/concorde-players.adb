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

   ---------------------
   -- Order_Move_Ship --
   ---------------------

   procedure Order_Move_Ship
     (Player      : in out Root_Player_Type'Class;
      Empire      : Concorde.Empires.Empire_Type;
      Ship        : Concorde.Ships.Ship_Type;
      Destination : Concorde.Systems.Star_System_Type)
   is
   begin
      Player.Orders.Append
        (Concorde.Orders.Ships.Move_Ship_Order
           (Empire, Ship, Destination));
   end Order_Move_Ship;

end Concorde.Players;
