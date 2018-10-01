with Concorde.Money;
with Concorde.Quantities;

with Concorde.Calendar;
with Concorde.Random;

with Concorde.Objects.Queues;

with Concorde.People.Individuals;
with Concorde.Worlds;

with Concorde.Managers.Ministries;
with Concorde.Powers.Ministries;

package body Concorde.Ministries.Create is

   function Create_Ministry
     (Faction  : Concorde.Factions.Faction_Type;
      Minister : Concorde.People.Individuals.Individual_Type;
      Area     : Concorde.Objects.Object_Type;
      Location : Concorde.People.Communities.Community_Type;
      Market   : Concorde.Markets.Market_Type;
      Name     : String;
      Budget   : Ministry_Budget;
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
        Minister => null,
        Area     => null,
        Location => Faction.Capital_Community,
        Market   => Faction.Capital_Community.Market,
        Name     => "House " & Faction.Name,
        Budget   => (List => Budget_Item_Lists.Empty_List),
        Powers   => Concorde.Powers.No_Powers));

   ---------------------
   -- Create_Ministry --
   ---------------------

   procedure Create_Ministry
     (Faction  : Concorde.Factions.Faction_Type;
      Area     : not null access constant
        Concorde.Laws.Law_Target_Interface'Class;
      Location : Concorde.People.Communities.Community_Type;
      Market   : Concorde.Markets.Market_Type;
      Name     : String;
      Budget   : Ministry_Budget;
      Powers   : Concorde.Powers.Power_Set)
   is
      Ministry : constant Ministry_Type :=
                   Create_Ministry
                     (Faction, null, Concorde.Objects.Object_Type (Area),
                      Location, Market, Name, Budget, Powers);
   begin
      Faction.Update.Add_Power
        (Concorde.Powers.Ministries.Direct_Minister (Ministry));
   end Create_Ministry;

   ---------------------
   -- Create_Ministry --
   ---------------------

   function Create_Ministry
     (Faction  : Concorde.Factions.Faction_Type;
      Minister : Concorde.People.Individuals.Individual_Type;
      Area     : Concorde.Objects.Object_Type;
      Location : Concorde.People.Communities.Community_Type;
      Market   : Concorde.Markets.Market_Type;
      Name     : String;
      Budget   : Ministry_Budget;
      Powers   : Concorde.Powers.Power_Set)
      return Ministry_Type
   is
      procedure Create (Ministry : in out Root_Ministry_Type'Class);

      procedure Transfer_Power
        (Power : Concorde.Powers.Power_Type);

      ------------
      -- Create --
      ------------

      procedure Create (Ministry : in out Root_Ministry_Type'Class) is
         use type Concorde.Objects.Object_Type;
      begin
         Ministry.New_Agent
           (Location       => Concorde.Locations.In_Community (Location),
            Government     => null,
            Market         => Market,
            Cash           => Concorde.Money.Zero,
            Stock_Capacity => Concorde.Quantities.Zero);

         Ministry.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
         Ministry.Minister := Minister;
         Ministry.Government := Location.Government;
         Ministry.Community := Location;
         Ministry.Daily_Budget := Budget.List;
         Ministry.Area :=
           (if Area = null
            then Concorde.Objects.Object_Type (Faction)
            else Area);
         Ministry.Set_Guarantor (Ministry.Government);

      end Create;

      Ministry : constant Ministry_Type := Db.Create (Create'Access);

      --------------------
      -- Transfer_Power --
      --------------------

      procedure Transfer_Power
        (Power : Concorde.Powers.Power_Type)
      is
      begin
         Faction.Update.Remove_Power (Power);
         Faction.First_Ministry.Update.Delegate_Power (Power, Ministry);
         Ministry.Update.Add_Power (Power);
      end Transfer_Power;

      use type Concorde.Calendar.Time;

   begin
      Faction.Update.Add_Ministry (Ministry);
      Powers.Scan_Powers (Transfer_Power'Access);

      Ministry.Save_Agent;
      Concorde.Managers.Ministries.Create_Manager (Ministry).Activate;
      Concorde.Objects.Queues.Next_Event
        (Ministry,
         Concorde.Calendar.Clock
         + Duration (Concorde.Random.Unit_Random * 86_400.0));
      return Ministry;
   end Create_Ministry;

end Concorde.Ministries.Create;
