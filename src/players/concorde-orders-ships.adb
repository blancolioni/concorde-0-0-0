with Concorde.Ships.Db;

package body Concorde.Orders.Ships is

   type Move_Ship_Order_Record is
     new Root_Order_Type with
      record
         Empire      : Concorde.Empires.Empire_Type;
         Ship        : Concorde.Ships.Ship_Type;
         Destination : Concorde.Systems.Star_System_Type;
      end record;

   overriding procedure Execute
     (Order : Move_Ship_Order_Record);

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Order : Move_Ship_Order_Record)
   is
      use type Concorde.Empires.Empire_Type;

      procedure Set_Destination
        (Ship : in out Concorde.Ships.Root_Ship_Type'Class);

      ---------------------
      -- Set_Destination --
      ---------------------

      procedure Set_Destination
        (Ship : in out Concorde.Ships.Root_Ship_Type'Class)
      is
      begin
         Ship.Set_Destination (Order.Destination);
      end Set_Destination;

   begin
      if Order.Ship.Alive
        and then Order.Ship.Owner = Order.Empire
      then
         Concorde.Ships.Db.Update
           (Order.Ship.Reference, Set_Destination'Access);
      end if;
   end Execute;

   ---------------------
   -- Move_Ship_Order --
   ---------------------

   function Move_Ship_Order
     (Empire      : not null access constant
        Concorde.Empires.Root_Empire_Type'Class;
      Ship        : not null access constant
        Concorde.Ships.Root_Ship_Type'Class;
      Destination : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Root_Order_Type'Class
   is
   begin
      return Result : Move_Ship_Order_Record do
         Result.Empire := Concorde.Empires.Empire_Type (Empire);
         Result.Ship := Concorde.Ships.Ship_Type (Ship);
         Result.Destination := Concorde.Systems.Star_System_Type (Destination);
      end return;
   end Move_Ship_Order;

end Concorde.Orders.Ships;
