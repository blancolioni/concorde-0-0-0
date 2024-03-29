package body Concorde.Orders.Ships is

   type Root_Ship_Order_Record is
     abstract new Root_Order_Type with
      record
         Empire      : Concorde.Empires.Empire_Type;
         Ship        : Concorde.Ships.Ship_Type;
      end record;

   type Move_Ship_Order_Record is
     new Root_Ship_Order_Record with
      record
         Destination : Concorde.Worlds.World_Type;
      end record;

   overriding procedure Execute
     (Order : Move_Ship_Order_Record);

   type Move_Ship_To_System_Record is
     new Root_Ship_Order_Record with
      record
         Destination : Concorde.Systems.Star_System_Type;
      end record;

   overriding procedure Execute
     (Order : Move_Ship_To_System_Record);

   type Colonise_Order_Record is
     new Root_Ship_Order_Record with
      record
         null;
      end record;

   overriding procedure Execute
     (Order : Colonise_Order_Record);

   --------------------
   -- Colonise_Order --
   --------------------

   function Colonise_Order
     (Empire      : not null access constant
        Concorde.Empires.Root_Empire_Type'Class;
      Ship        : not null access constant
        Concorde.Ships.Root_Ship_Type'Class)
      return Root_Order_Type'Class
   is
   begin
      return Result : Colonise_Order_Record do
         Result.Empire := Concorde.Empires.Empire_Type (Empire);
         Result.Ship := Concorde.Ships.Ship_Type (Ship);
      end return;
   end Colonise_Order;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Order : Move_Ship_Order_Record)
   is
      use type Concorde.Empires.Empire_Type;

   begin
      if Order.Ship.Alive
        and then Order.Ship.Owner = Order.Empire
      then
         Order.Ship.Update.Set_Destination (Order.Destination);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Order : Move_Ship_To_System_Record)
   is
      use type Concorde.Empires.Empire_Type;

   begin
      if Order.Ship.Alive
        and then Order.Ship.Owner = Order.Empire
      then
         Order.Ship.Update.Set_Destination (Order.Destination);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Order : Colonise_Order_Record)
   is
   begin
      Order.Ship.Update.Set_Colonisation_Order;
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
        Concorde.Worlds.Root_World_Type'Class)
      return Root_Order_Type'Class
   is
   begin
      return Result : Move_Ship_Order_Record do
         Result.Empire := Concorde.Empires.Empire_Type (Empire);
         Result.Ship := Concorde.Ships.Ship_Type (Ship);
         Result.Destination := Concorde.Worlds.World_Type (Destination);
      end return;
   end Move_Ship_Order;

   -------------------------
   -- Move_Ship_To_System --
   -------------------------

   function Move_Ship_To_System
     (Empire      : not null access constant
        Concorde.Empires.Root_Empire_Type'Class;
      Ship        : not null access constant
        Concorde.Ships.Root_Ship_Type'Class;
      Destination : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Root_Order_Type'Class
   is
   begin
      return Result : Move_Ship_To_System_Record do
         Result.Empire := Concorde.Empires.Empire_Type (Empire);
         Result.Ship := Concorde.Ships.Ship_Type (Ship);
         Result.Destination := Concorde.Systems.Star_System_Type (Destination);
      end return;
   end Move_Ship_To_System;

end Concorde.Orders.Ships;
