with Concorde.Dates;
with Concorde.Random;
with Concorde.Quantities;

with Concorde.Objects.Queues;

with Concorde.Managers.Installations;

package body Concorde.Installations.Create is

   ------------
   -- Create --
   ------------

   function Create
     (Location      : Concorde.Locations.Object_Location;
      Market        : access constant Concorde.Trades.Trade_Interface'Class;
      Facility      : Concorde.Facilities.Facility_Type;
      Cash          : Concorde.Money.Money_Type;
      Owner         : not null access constant
        Concorde.Agents.Root_Agent_Type'Class)
      return Installation_Type
   is

      procedure Initialise
        (Installation : in out Root_Installation_Type'Class);

      ----------------
      -- Initialise --
      ----------------

      procedure Initialise
        (Installation : in out Root_Installation_Type'Class)
      is
         use Concorde.Quantities;
         Storage : constant Quantity_Type :=
                     Facility.Capacity_Quantity
                       * To_Quantity (100.0);
      begin
         Installation.New_Agent (Location, Market, Storage);
         Installation.Facility := Facility;
         Installation.Owner := Owner;
         Installation.Set_Cash (Cash);
         Installation.Set_Guarantor (Owner);
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
            Concorde.Dates.Add_Seconds
              (Concorde.Dates.Current_Date,
               Concorde.Random.Unit_Random * 86_400.0));
      end return;
   end Create;

end Concorde.Installations.Create;
