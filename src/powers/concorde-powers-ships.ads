with Concorde.Ships;

package Concorde.Powers.Ships is

   function Appoint_Trader_Captain return Power_Type;

   function Captain_Trader_Ship
     (Ship : Concorde.Ships.Ship_Type)
      return Power_Type;

end Concorde.Powers.Ships;
