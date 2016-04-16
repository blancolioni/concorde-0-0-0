with Concorde.Installations.Db;

package body Concorde.Installations.Create is

   ------------
   -- Create --
   ------------

   function Create
     (Location : not null access constant
        Concorde.Agents.Agent_Location_Interface'Class;
      Facility : Concorde.Facilities.Facility_Type;
      Cash     : Concorde.Money.Money_Type;
      Owner    : not null access constant
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
      begin
         Installation.New_Agent (Location);
         Installation.Facility := Facility;
         Installation.Owner := Owner;
         Installation.Set_Cash (Cash);
      end Initialise;

   begin
      return Concorde.Installations.Db.Create
        (Initialise'Access);
   end Create;

end Concorde.Installations.Create;
