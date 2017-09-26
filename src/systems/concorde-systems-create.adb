with Concorde.Random;
with Concorde.Scenarios;

with Concorde.Commodities;
with Concorde.Stars.Create;

package body Concorde.Systems.Create is

   function Random_Star_Mass return Non_Negative_Real;

   ----------------
   -- New_System --
   ----------------

   function New_System
     (Index       : Positive;
      Name        : String;
      X, Y, Z     : Real;
      Primary     : Concorde.Stars.Star_Type := null)
     return Star_System_Type
   is
      use Concorde.Commodities;

--        Resources : constant Concorde.Commodities.Array_Of_Commodities :=
--                      Concorde.Commodities.Get
--                        (Concorde.Commodities.Organic);

      Imperial_Centre : constant Boolean :=
                          Concorde.Scenarios.Imperial_Centre
                              and then Index = 1;

      procedure Create (System : in out Root_Star_System_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (System : in out Root_Star_System_Type'Class) is
--           Resource : constant Concorde.Commodities.Commodity_Type :=
--                        Resources
--                          (WL.Random.Random_Number
--                             (Resources'First, Resources'Last));
      begin
         System.Set_Name (Name);
         System.Index := Index;
         System.X := X;
         System.Y := Y;
         System.Z := Z;

         System.Boundary := new System_Influence_Boundary (1 .. 0);

         declare
            use type Concorde.Stars.Star_Type;
            Main_Star : constant Concorde.Stars.Star_Type :=
                          (if Primary = null
                           then Concorde.Stars.Create.New_Main_Sequence_Star
                             (Name,
                              (if Imperial_Centre
                               then 1.0
                               else Random_Star_Mass))
                           else Primary);
         begin
            System.Add_Object
              (Object   => Main_Star);
         end;

      end Create;

      System : constant Star_System_Type :=
                 Concorde.Systems.Db.Create (Create'Access);

   begin

      System_Vector.Append (System);

      return System;

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
