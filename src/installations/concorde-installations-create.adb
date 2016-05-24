with Concorde.Quantities;

with Concorde.Installations.Db;

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
         Storage : constant Quantity :=
                     Facility.Capacity_Quantity
                       * To_Quantity (100.0);
      begin
         Installation.New_Agent (Location, Market, Storage);
         Installation.Facility := Facility;
         Installation.Owner := Owner;
         Installation.Set_Cash (Cash);
      end Initialise;

   begin
      return Concorde.Installations.Db.Create
        (Initialise'Access);
   end Create;

end Concorde.Installations.Create;
