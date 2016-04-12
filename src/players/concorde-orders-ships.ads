with Concorde.Empires;
with Concorde.Ships;
with Concorde.Systems;

package Concorde.Orders.Ships is

   function Move_Ship_Order
     (Empire      : not null access constant
        Concorde.Empires.Root_Empire_Type'Class;
      Ship        : not null access constant
        Concorde.Ships.Root_Ship_Type'Class;
      Destination : not null access constant
        Concorde.Systems.Root_Star_System_Type'Class)
      return Root_Order_Type'Class;

end Concorde.Orders.Ships;
