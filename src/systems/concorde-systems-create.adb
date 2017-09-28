--  with Concorde.Commodities;

package body Concorde.Systems.Create is

   ----------------
   -- New_System --
   ----------------

   function New_System
     (Index       : Positive;
      Name        : String;
      X, Y, Z     : Real)
     return Star_System_Type
   is
--        use Concorde.Commodities;

--        Resources : constant Concorde.Commodities.Array_Of_Commodities :=
--                      Concorde.Commodities.Get
--                        (Concorde.Commodities.Organic);

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

      end Create;

      System : constant Star_System_Type :=
                 Concorde.Systems.Db.Create (Create'Access);

   begin

      System_Vector.Append (System);

      return System;

   end New_System;

end Concorde.Systems.Create;
