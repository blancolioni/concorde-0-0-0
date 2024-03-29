with WL.Localisation;

package body Concorde.Powers.Ships is

   type Appoint_Captain_Power is
     abstract new Root_Power_Type with null record;

   type Appoint_Trader_Captain_Power is
     new Appoint_Captain_Power with null record;

   overriding function Class_Identifier
     (Power : Appoint_Trader_Captain_Power)
      return String
   is ("appoint_trader_captain");

   type Captain_Ship_Power is
     abstract new Root_Power_Type with
      record
         Ship : Concorde.Ships.Ship_Type;
      end record;

   type Captain_Trader_Ship_Power is
     new Captain_Ship_Power with null record;

   overriding function Class_Identifier
     (Power : Captain_Trader_Ship_Power)
      return String
   is ("captain_trader_ship");

   overriding function Identifier
     (Power : Captain_Trader_Ship_Power)
      return String
   is ("captain_trader_ship_" & Power.Ship.Identifier);

   overriding function Show
     (Power : Captain_Trader_Ship_Power)
      return String
   is (WL.Localisation.Local_Text
       ("captain_trader_ship", Power.Ship.Name));

   ----------------------------
   -- Appoint_Trader_Captain --
   ----------------------------

   function Appoint_Trader_Captain return Power_Type is
   begin
      return Power : Appoint_Trader_Captain_Power;
   end Appoint_Trader_Captain;

   ------------------
   -- Captain_Ship --
   ------------------

   function Captain_Trader_Ship
     (Ship : not null access constant
        Concorde.Ships.Root_Ship_Type'Class)
      return Power_Type
   is
   begin
      return Captain_Trader_Ship_Power'
        (Ship => Concorde.Ships.Ship_Type (Ship));
   end Captain_Trader_Ship;

end Concorde.Powers.Ships;
