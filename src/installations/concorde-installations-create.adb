with Concorde.Installations.Db;

package body Concorde.Installations.Create is

   ------------
   -- Create --
   ------------

   function Create
     (Facility : Concorde.Facilities.Facility_Type;
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
         Installation.Facility := Facility;
         Installation.Owner := Owner;
      end Initialise;

   begin
      return Concorde.Installations.Db.Create
        (Initialise'Access);
   end Create;

end Concorde.Installations.Create;