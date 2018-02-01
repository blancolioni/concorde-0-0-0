with WL.Money;
with WL.Quantities;

with Concorde.Calendar;
with Concorde.Random;

with Concorde.Objects.Queues;

with Concorde.Managers.Ministries;

package body Concorde.Ministries.Create is

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
      procedure Create (Ministry : in out Root_Ministry_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Ministry : in out Root_Ministry_Type'Class) is
      begin
         Ministry.New_Agent
           (Location       => Concorde.Locations.At_Installation (Location),
            Government     => null,
            Market         => Market,
            Cash           => WL.Money.Zero,
            Stock_Capacity => WL.Quantities.Zero);

         Ministry.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
         Ministry.Area := Concorde.Objects.Object_Type (Area);
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
   end Create_Ministry;

end Concorde.Ministries.Create;
