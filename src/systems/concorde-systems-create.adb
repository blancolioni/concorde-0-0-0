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

      procedure Create (System : in out Root_Star_System_Type'Class);

      ------------
      -- Create --
      ------------

      procedure Create (System : in out Root_Star_System_Type'Class) is
      begin
         System.Set_Name (Name);
         System.Index := Index;
         System.X := X;
         System.Y := Y;
         System.Production := Production;
         System.Capacity := Capacity;
         System.Boundary :=
           new System_Influence_Boundary'(Boundary);
      end Create;

   begin
      return Concorde.Systems.Db.Create (Create'Access);
   end New_System;

end Concorde.Systems.Create;
