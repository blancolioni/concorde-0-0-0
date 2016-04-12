package Concorde.Orders is

   type Executive_Interface is interface;

   type Executive_Access is access Executive_Interface'Class;

   type Root_Order_Type is abstract tagged private;

   procedure Execute
     (Order : Root_Order_Type)
   is abstract;

private

   type Root_Order_Type is abstract tagged null record;

end Concorde.Orders;
