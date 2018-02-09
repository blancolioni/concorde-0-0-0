with WL.Money;
with WL.Quantities;

with Concorde.Factions;
with Concorde.Powers;
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
         Army.Set_Name ("1st " & Faction.Name & " regulars");
         Army.Faction := Faction;
         Army.Loyalty := 1.0;

      end Create;

      Army : constant Army_Type := Db.Create (Create'Access);
   begin

      Update_Location (Army);

      return Army;
   end New_Army;

end Concorde.Armies.Create;
