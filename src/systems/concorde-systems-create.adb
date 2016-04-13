with WL.Random;

with Concorde.Random;

with Concorde.Commodities;

with Concorde.Systems.Db;

package body Concorde.Systems.Create is

   ----------------
   -- New_System --
   ----------------

   function New_System
     (Index      : Positive;
      Name       : String;
      X, Y       : Real;
      Boundary   : System_Influence_Boundary;
      Production : Non_Negative_Real;
      Capacity   : Non_Negative_Real)
      return Star_System_Type
   is

      Resources : constant Concorde.Commodities.Array_Of_Commodities :=
                    Concorde.Commodities.Get
                      (Concorde.Commodities.Resource);

      procedure Create (System : in out Root_Star_System_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (System : in out Root_Star_System_Type'Class) is
         Resource : constant Concorde.Commodities.Commodity_Type :=
                      Resources
                        (WL.Random.Random_Number
                           (Resources'First, Resources'Last));
      begin
         System.Set_Name (Name);
         System.Index := Index;
         System.X := X;
         System.Y := Y;
         System.Production := Production;
         System.Capacity := Capacity;
         System.Boundary :=
           new System_Influence_Boundary'(Boundary);
         System.Deposit :=
           (Resource => Resource,
            Accessibility => Concorde.Random.Unit_Random,
            Concentration => Concorde.Random.Unit_Random,
            Size          =>
              Concorde.Quantities.Around
                (Concorde.Quantities.To_Quantity (1.0E6)));
      end Create;

   begin
      return Concorde.Systems.Db.Create (Create'Access);
   end New_System;

end Concorde.Systems.Create;
