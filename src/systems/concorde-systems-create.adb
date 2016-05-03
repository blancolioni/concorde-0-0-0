with Ada.Text_IO;

with WL.Random;

with Concorde.Random;
with Concorde.Scenarios;

with Concorde.Commodities;
with Concorde.Stars.Create;
with Concorde.Worlds.Create;

with Concorde.Systems.Db;

package body Concorde.Systems.Create is

   function Random_Star_Mass return Non_Negative_Real;

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
      use Concorde.Commodities;

      Resources : constant Concorde.Commodities.Array_Of_Commodities :=
                    Concorde.Commodities.Get
                      (Concorde.Commodities.Organic);

      Imperial_Centre : constant Boolean :=
                          Concorde.Scenarios.Imperial_Centre
                              and then Index = 1;

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

         declare
            Main_Star : constant Concorde.Stars.Star_Type :=
                          Concorde.Stars.Create.New_Main_Sequence_Star
                            (Name,
                             (if True or else Imperial_Centre
                              then 1.0
                              else Random_Star_Mass));
         begin
            System.Add_Object
              (Object   => Main_Star,
               Primary  => null,
               Orbit    => 0.0,
               Position => Concorde.Geometry.Degrees_To_Radians (0.0));
            Ada.Text_IO.Put ("Star: " & Main_Star.Name);
         end;

         declare
            Deposit_Size : constant Concorde.Quantities.Quantity :=
                             Concorde.Quantities.Around
                               (Concorde.Quantities.To_Quantity
                                  ((if Imperial_Centre
                                   then 1.0e2 else 1.0E6)));
            Accessibility : constant Unit_Real :=
                              (if Imperial_Centre
                               then Concorde.Random.Unit_Random / 10.0
                               else Concorde.Random.Unit_Random / 2.0 + 0.3);
            Concentration : constant Unit_Real :=
                              (if Imperial_Centre
                               then Concorde.Random.Unit_Random / 10.0
                               else Concorde.Random.Unit_Random / 2.0 + 0.3);
         begin
            System.Deposit :=
              (Resource      => Resource,
               Accessibility => Accessibility,
               Concentration => Concentration,
               Size          => Deposit_Size,
               Original_Size => Deposit_Size);
         end;

         Concorde.Worlds.Create.Create_Worlds
           (System);

      end Create;

   begin
      return Concorde.Systems.Db.Create (Create'Access);
   end New_System;

   ----------------------
   -- Random_Star_Mass --
   ----------------------

   function Random_Star_Mass return Non_Negative_Real is
      Seed : constant Unit_Real := Concorde.Random.Unit_Random;
      Solar_Mass_Count : Real;
   begin
      if Seed <= 0.99 then
         Solar_Mass_Count :=
           0.1 + 6.0 * Seed - 15.0 * Seed ** 2
             + 11.0 * Seed ** 3;
      else
         declare
            X : constant Real := (Seed - 0.99) * 1.0E4;
            A : constant Real := 0.110833;
            B : constant Real := -14.0358;
            C : constant Real := 445.25;
         begin
            Solar_Mass_Count := A * X ** 2 + B * X + C;
         end;
      end if;
      return Solar_Mass_Count;
   end Random_Star_Mass;

end Concorde.Systems.Create;
