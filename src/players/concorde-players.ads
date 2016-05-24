private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Concorde.Orders;

with Concorde.Empires;
with Concorde.Ships;
with Concorde.Worlds;
with Concorde.Systems;

package Concorde.Players is

   type Root_Player_Type is abstract new Concorde.Orders.Executive_Interface
   with private;

   procedure On_Start
     (Player : in out Root_Player_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class)
   is abstract;

   procedure On_System_Colonised
     (Player : in out Root_Player_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class;
      System : Concorde.Systems.Root_Star_System_Type'Class;
      Ship   : not null access constant
        Concorde.Ships.Root_Ship_Type'Class)
   is null;

   procedure On_System_Captured
     (Player : in out Root_Player_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class;
      System : Concorde.Systems.Root_Star_System_Type'Class;
      From   : Concorde.Empires.Empire_Type)
   is null;

   procedure On_System_Lost
     (Player : in out Root_Player_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class;
      System : Concorde.Systems.Root_Star_System_Type'Class;
      To     : Concorde.Empires.Empire_Type)
   is null;

   procedure On_Ship_Completed
     (Player : in out Root_Player_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class;
      Ship   : Concorde.Ships.Ship_Type)
   is abstract;

   procedure On_Ship_Arrived
     (Player : in out Root_Player_Type;
      Empire : not null access constant
        Concorde.Empires.Root_Empire_Type'Class;
      Ship   : Concorde.Ships.Ship_Type)
   is abstract
     with Pre'Class => not Ship.Has_Destination;

   procedure On_Ship_Destroyed
     (Player : in out Root_Player_Type;
      Empire : in out Concorde.Empires.Root_Empire_Type'Class;
      Ship   : Concorde.Ships.Ship_Type)
   is null;

   procedure Order_Move_Ship
     (Player      : in out Root_Player_Type'Class;
      Empire      : Concorde.Empires.Empire_Type;
      Ship        : Concorde.Ships.Ship_Type;
      Destination : Concorde.Worlds.World_Type)
     with Pre => not Ship.Has_Orders;

   procedure Order_Explore_System
     (Player      : in out Root_Player_Type'Class;
      Empire      : Concorde.Empires.Empire_Type;
      Ship        : Concorde.Ships.Ship_Type;
      Destination : Concorde.Systems.Star_System_Type)
     with Pre => not Ship.Has_Orders;

   procedure Order_Colonisation
     (Player      : in out Root_Player_Type'Class;
      Empire      : not null access constant
        Concorde.Empires.Root_Empire_Type'Class;
      Ship        : Concorde.Ships.Ship_Type)
     with Pre => not Ship.Has_Orders;

   procedure Execute_Orders
     (Player : in out Root_Player_Type'Class);

   type Player_Type is access all Root_Player_Type'Class;

private

   package List_Of_Orders is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Concorde.Orders.Root_Order_Type'Class,
        Concorde.Orders."=");

   type Root_Player_Type is
     abstract new Concorde.Orders.Executive_Interface with
      record
         Orders      : List_Of_Orders.List;
      end record;

end Concorde.Players;
