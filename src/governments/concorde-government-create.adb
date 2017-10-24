with Concorde.Locations;

package body Concorde.Government.Create is

   -----------------------
   -- Create_Government --
   -----------------------

   function Create_Government
     (Governed          : not null access constant Governed_Interface'Class;
      Cash              : WL.Money.Money_Type;
      Owner             : not null access constant
        Concorde.Agents.Root_Agent_Type'Class;
      Headquarters      : not null access constant
        Concorde.Installations.Root_Installation_Type'Class;
      Basic_Living_Wage : WL.Money.Price_Type)
      return Government_Type
   is
      procedure Create (Government : in out Root_Government_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Government : in out Root_Government_Type'Class) is
      begin
         Government.New_Agent
           (Concorde.Locations.At_Installation (Headquarters),
            null, null,
            WL.Quantities.Zero);
         Government.Governed := Governed;
         Government.Set_Cash (Cash);
         Government.Headquarters :=
           Concorde.Installations.Installation_Type (Headquarters);
         Government.Owner := Owner;
         Government.Set_Guarantor (Owner);
         Government.Basic_Living_Wage := Basic_Living_Wage;
      end Create;

   begin
      return Concorde.Government.Db.Create
        (Create'Access);
   end Create_Government;

end Concorde.Government.Create;
