with Concorde.Elementary_Functions;

package body Concorde.Systems is

   ------------
   -- Attack --
   ------------

   procedure Attacked
     (System   : in out Root_Star_System_Type'Class;
      Attacker : Star_System_Type)
   is
   begin
      System.Last_Battle := Concorde.Dates.Current_Date;
      System.Last_Attacker := Attacker;
   end Attacked;

   --------------
   -- Capacity --
   --------------

   function Capacity (System : Root_Star_System_Type'Class)
                      return Non_Negative_Real
   is
   begin
      return System.Capacity;
   end Capacity;

   -------------
   -- Capital --
   -------------

   function Capital (System : Root_Star_System_Type'Class)
                     return Boolean
   is
   begin
      return System.Capital;
   end Capital;

   --------------
   -- Distance --
   --------------

   function Distance
     (System_1, System_2 : Star_System_Type)
      return Non_Negative_Real
   is
      function Sqrt (X : Non_Negative_Real) return Non_Negative_Real
                     renames Concorde.Elementary_Functions.Sqrt;
   begin
      return Sqrt ((System_1.X - System_2.X) ** 2
                   + (System_1.Y - System_2.Y) ** 2);
   end Distance;

   ------------
   -- Fleets --
   ------------

   function Fleets (System : Root_Star_System_Type'Class)
                    return Natural
   is
   begin
      return System.Fleets;
   end Fleets;

   -----------
   -- Index --
   -----------

   function Index (System : Root_Star_System_Type'Class) return Positive is
   begin
      return System.Index;
   end Index;

   -----------------
   -- Last_Attack --
   -----------------

   function Last_Attacker
     (From : Root_Star_System_Type'Class)
      return Star_System_Type
   is
   begin
      return From.Last_Attacker;
   end Last_Attacker;

   -----------------
   -- Last_Battle --
   -----------------

   function Last_Battle
     (System : Root_Star_System_Type'Class)
      return Concorde.Dates.Date_Type
   is
   begin
      return System.Last_Battle;
   end Last_Battle;

   -----------
   -- Owner --
   -----------

   function Owner
     (System : Root_Star_System_Type'Class)
      return access Concorde.Empires.Root_Empire_Type'Class
   is
   begin
      return System.Owner;
   end Owner;

   ----------------
   -- Production --
   ----------------

   function Production (System : Root_Star_System_Type'Class)
                        return Non_Negative_Real
   is
   begin
      return System.Production;
   end Production;

   ------------------
   -- Set_Capacity --
   ------------------

   procedure Set_Capacity
     (System : in out Root_Star_System_Type'Class;
      New_Capacity : Non_Negative_Real)
   is
   begin
      System.Capacity := New_Capacity;
   end Set_Capacity;

   -----------------
   -- Set_Capital --
   -----------------

   procedure Set_Capital
     (System : in out Root_Star_System_Type'Class)
   is
   begin
      System.Capital := True;
   end Set_Capital;

   ----------------
   -- Set_Fleets --
   ----------------

   procedure Set_Fleets
     (System     : in out Root_Star_System_Type'Class;
      New_Fleets : Natural)
   is
   begin
      System.Fleets := New_Fleets;
   end Set_Fleets;

   ---------------
   -- Set_Owner --
   ---------------

   procedure Set_Owner
     (System : in out Root_Star_System_Type'Class;
      New_Owner : not null access
        Concorde.Empires.Root_Empire_Type'Class)
   is
   begin
      System.Owner := New_Owner;
   end Set_Owner;

   --------------------
   -- Set_Production --
   --------------------

   procedure Set_Production
     (System : in out Root_Star_System_Type'Class;
      New_Production : Non_Negative_Real)
   is
   begin
      System.Production := New_Production;
   end Set_Production;

   -------
   -- X --
   -------

   function X (System : Root_Star_System_Type'Class) return Real is
   begin
      return System.X;
   end X;

   -------
   -- Y --
   -------

   function Y (System : Root_Star_System_Type'Class) return Real is
   begin
      return System.Y;
   end Y;

end Concorde.Systems;
