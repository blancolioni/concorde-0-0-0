with Concorde.Locations;

package body Concorde.Government.Create is

   -----------------------
   -- Create_Government --
   -----------------------

   function Create_Government
     (Governed : not null access constant Governed_Interface'Class;
      Location : Concorde.Locations.Object_Location;
      Cash     : Concorde.Money.Money_Type;
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
            Cash, Concorde.Quantities.Zero);
         Government.Governed := Governed;
         Government.Owner := Owner;
         Government.Set_Guarantor (Owner);
         Government.Add_Income_Tax_Rate
           (Lower_Bound => Concorde.Money.To_Price (0.5),
            Rate        => 0.1);
         Government.Add_Income_Tax_Rate
           (Lower_Bound => Concorde.Money.To_Price (1.5),
            Rate        => 0.2);
         Government.Add_Income_Tax_Rate
           (Lower_Bound => Concorde.Money.To_Price (4.5),
            Rate        => 0.2);
         Government.Add_Income_Tax_Rate
           (Lower_Bound => Concorde.Money.To_Price (9.5),
            Rate        => 0.2);
         Government.Add_Income_Tax_Rate
           (Lower_Bound => Concorde.Money.To_Price (19.5),
            Rate        => 0.2);
      end Create;

   begin
      return Concorde.Government.Db.Create
        (Create'Access);
   end Create_Government;

end Concorde.Government.Create;
