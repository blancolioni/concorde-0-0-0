with Concorde.Calendar;
with Concorde.Random;
with WL.Quantities;

with Concorde.Objects.Queues;

with Concorde.Government;

with Concorde.Managers.Installations;

package body Concorde.Installations.Create is

   ------------
   -- Create --
   ------------

   function Create
     (Location      : Concorde.Locations.Object_Location;
      Market        : access constant Concorde.Trades.Trade_Interface'Class;
      Facility      : Concorde.Facilities.Facility_Type;
      Cash          : WL.Money.Money_Type;
      Owner         : not null access constant
        Concorde.Agents.Root_Agent_Type'Class)
      return Installation_Type
   is

      use type Concorde.Calendar.Time;

      procedure Initialise
        (Installation : in out Root_Installation_Type'Class);

      ----------------
      -- Initialise --
      ----------------

      procedure Initialise
        (Installation : in out Root_Installation_Type'Class)
      is
         use WL.Quantities;
         Storage : constant Quantity_Type :=
                     Facility.Capacity_Quantity
                       * To_Quantity (100.0);
      begin
         Installation.New_Agent
           (Location       => Location,
            Government     => Concorde.Government.Get_Government (Location),
            Market         => Market,
            Stock_Capacity => Storage);
         Installation.Facility := Facility;
         Installation.Owner := Owner;
         Installation.Set_Cash (Cash);
         Installation.Set_Guarantor (Owner);
         Installation.Contracted_Buys.Create_Stock
           (Storage, True);
      end Initialise;

   begin
      return Installation : constant Installation_Type :=
        Db.Create (Initialise'Access)
      do
         Installation.Save_Agent;
         Concorde.Managers.Installations.Create_Manager
           (Installation).Activate;
         Concorde.Objects.Queues.Next_Event
           (Installation,
            Concorde.Calendar.Clock
            + Duration (Concorde.Random.Unit_Random * 86_400.0));
      end return;
   end Create;

end Concorde.Installations.Create;
