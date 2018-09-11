with Concorde.Locations;

package body Concorde.Government.Create is

   -----------------------
   -- Create_Government --
   -----------------------

   function Create_Government
     (Governed : not null access constant Governed_Interface'Class;
      Location : Concorde.Locations.Object_Location;
      Cash     : WL.Money.Money_Type;
      Owner    : not null access constant
        Concorde.Agents.Root_Agent_Type'Class)
      return Government_Type
   is
      procedure Create (Government : in out Root_Government_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Government : in out Root_Government_Type'Class) is
      begin
         Government.New_Agent
           (Location,
            null, null,
            Cash, WL.Quantities.Zero);
         Government.Governed := Governed;
         Government.Owner := Owner;
         Government.Set_Guarantor (Owner);
      end Create;

   begin
      return Concorde.Government.Db.Create
        (Create'Access);
   end Create_Government;

end Concorde.Government.Create;
