with WL.Money;
with WL.Quantities;

with Concorde.Calendar;
with Concorde.Random;

with Concorde.Objects.Queues;

with Concorde.People.Individuals;
with Concorde.Worlds;

with Concorde.Managers.Ministries;

package body Concorde.Ministries.Create is

   function Create_Ministry
     (Faction  : Concorde.Factions.Faction_Type;
      Minister : Concorde.People.Individuals.Individual_Type;
      Area     : Concorde.Objects.Object_Type;
      Location : Concorde.Installations.Installation_Type;
      Market   : Concorde.Markets.Market_Type;
      Name     : String;
      Powers   : Concorde.Powers.Power_Set)
      return Ministry_Type;

   -----------------------------
   -- Create_Faction_Ministry --
   -----------------------------

   function Create_Faction_Ministry
     (Faction : Concorde.Factions.Faction_Type)
      return Ministry_Type
   is (Create_Ministry
       (Faction  => Faction,
        Minister => Faction.Leader,
        Area     => null,
        Location => Faction.Capital_Building,
        Market   => Faction.Capital_World.Market,
        Name     => "House " & Faction.Name,
        Powers   => Concorde.Powers.No_Powers));

   ---------------------
   -- Create_Ministry --
   ---------------------

   procedure Create_Ministry
     (Faction  : Concorde.Factions.Faction_Type;
      Area     : not null access constant
        Concorde.Objects.Root_Object_Type'Class;
      Location : Concorde.Installations.Installation_Type;
      Market   : Concorde.Markets.Market_Type;
      Name     : String;
      Powers   : Concorde.Powers.Power_Set)
   is
      Ministry : constant Ministry_Type :=
                   Create_Ministry
                     (Faction, null, Concorde.Objects.Object_Type (Area),
                      Location, Market, Name, Powers);
   begin
      pragma Unreferenced (Ministry);
   end Create_Ministry;

   ---------------------
   -- Create_Ministry --
   ---------------------

   function Create_Ministry
     (Faction  : Concorde.Factions.Faction_Type;
      Minister : Concorde.People.Individuals.Individual_Type;
      Area     : Concorde.Objects.Object_Type;
      Location : Concorde.Installations.Installation_Type;
      Market   : Concorde.Markets.Market_Type;
      Name     : String;
      Powers   : Concorde.Powers.Power_Set)
      return Ministry_Type
   is
      procedure Create (Ministry : in out Root_Ministry_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Ministry : in out Root_Ministry_Type'Class) is
         use type Concorde.Objects.Object_Type;
      begin
         Ministry.New_Agent
           (Location       => Concorde.Locations.At_Installation (Location),
            Government     => null,
            Market         => Market,
            Cash           => WL.Money.Zero,
            Stock_Capacity => WL.Quantities.Zero);

         Ministry.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
         Ministry.Minister := Minister;
         Ministry.Area :=
           (if Area = null
            then Concorde.Objects.Object_Type (Faction)
            else Area);
         Ministry.Headquarters := Location;
         Ministry.Powers := Powers;
      end Create;

      Ministry : constant Ministry_Type := Db.Create (Create'Access);

      use type Concorde.Calendar.Time;

   begin
      Faction.Update.Add_Ministry (Ministry);
      Ministry.Save_Agent;
      Concorde.Managers.Ministries.Create_Manager (Ministry).Activate;
      Concorde.Objects.Queues.Next_Event
        (Ministry,
         Concorde.Calendar.Clock
         + Duration (Concorde.Random.Unit_Random * 86_400.0));
      return Ministry;
   end Create_Ministry;

end Concorde.Ministries.Create;
