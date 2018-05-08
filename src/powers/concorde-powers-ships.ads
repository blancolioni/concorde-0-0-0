with Concorde.Ships;

package Concorde.Powers.Ships is

   function Appoint_Trader_Captain return Power_Type;

   function Captain_Trader_Ship
     (Ship : not null access constant
        Concorde.Ships.Root_Ship_Type'Class)
      return Power_Type;

end Concorde.Powers.Ships;
