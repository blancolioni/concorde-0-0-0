with WL.Money;
with WL.Quantities;

with Concorde.Factions;
with Concorde.Worlds;

package body Concorde.Armies.Create is

   --------------
   -- New_Army --
   --------------

   function New_Army
     (Faction : not null access constant
        Concorde.Factions.Root_Faction_Type'Class;
      Location : Concorde.Locations.Object_Location)
      return Army_Type
   is
      procedure Create (Army : in out Root_Army_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Army : in out Root_Army_Type'Class) is
      begin
         Army.New_Agent
           (Location       => Location,
            Government     => Faction.Capital_World.Government,
            Market         => Faction.Capital_World.Market,
            Cash           => WL.Money.Zero,
            Stock_Capacity => WL.Quantities.Zero);
         Army.Faction := Faction;
         Army.Location := Location;
         Army.Loyalty := 1.0;
      end Create;

   begin
      return Db.Create (Create'Access);
   end New_Army;

end Concorde.Armies.Create;
